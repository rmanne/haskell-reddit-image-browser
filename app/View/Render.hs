{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module View.Render
  ( newThread
  , Action(Stop, Resize)
  ) where

import qualified Codec.FFmpeg as FFmpeg
import qualified Codec.Picture.Types as Pic
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Lens ((.=), makeLenses, use)
import Control.Monad.Except (when)
import Control.Monad.Extra (whileM)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad.Trans.State.Strict (StateT)
import Data.Vector.Storable.ByteString (vectorToByteString)
import Foreign.C.Types (CInt)
import qualified SDL

data Action
  = Stop
  | Resize CInt CInt
  | Next
  | Render

type Pixel = Pic.PixelRGB8

data State =
  State
    { _rectangle :: SDL.Rectangle CInt
    , _texture :: SDL.Texture
    , _frame :: Pic.Image Pixel
    }

$(makeLenses ''State)

rectangleFor :: Pic.Image Pixel -> CInt -> CInt -> SDL.Rectangle CInt
rectangleFor Pic.Image { Pic.imageWidth = imageWidth
                       , Pic.imageHeight = imageHeight
                       } windowWidth windowHeight =
  let maxScale :: Double
      maxScale =
        max
          (fromIntegral imageWidth / fromIntegral windowWidth)
          (fromIntegral imageHeight / fromIntegral windowHeight)
      dimensions :: SDL.V2 CInt
      dimensions =
        SDL.V2
          (floor (fromIntegral imageWidth / maxScale))
          (floor (fromIntegral imageHeight / maxScale))
      toDouble :: CInt -> Double
      toDouble = fromIntegral
      offset :: SDL.V2 CInt
      offset =
        floor . (/ 2) . toDouble <$>
        (SDL.V2 windowWidth windowHeight - dimensions)
   in SDL.Rectangle (SDL.P offset) dimensions

-- TODO: scaling with something like bilinear scaling per-image is hella slow
-- look into libswscale (ffmpeg) and generally heavier bindings to ffmpeg
-- Additionally, might have to perform the scaling on a separate thread
-- http://hackage.haskell.org/package/hs-ffmpeg-0.3.4/src/src/Media/FFMpeg/SWScale.hsc
-- https://trac.ffmpeg.org/wiki/Using%20libav*
scale :: StateT State IO ()
scale =
  use texture >>= \t ->
    use frame >>= \f ->
      SDL.updateTexture
        t
        Nothing
        (vectorToByteString (Pic.imageData f))
        (fromIntegral $ Pic.imageWidth f * 3) >>
      return ()

allocate :: SDL.Renderer -> CInt -> CInt -> Pic.Image Pixel -> IO State
allocate renderer windowWidth windowHeight f =
  State (rectangleFor f windowWidth windowHeight) <$>
  SDL.createTexture
    renderer
    SDL.RGB24
    SDL.TextureAccessStreaming
    (SDL.V2 (fromIntegral $ Pic.imageWidth f) (fromIntegral $ Pic.imageHeight f)) <*>
  pure f

newThread :: SDL.Renderer -> FilePath -> CInt -> CInt -> Chan Action -> IO ()
newThread renderer file originalWindowWidth originalWindowHeight channel = do
  (reader, cleanup) <- FFmpeg.imageReaderTime (FFmpeg.File file)
  initialState <-
    reader >>= \case
      Just (firstFrame, _) ->
        allocate renderer originalWindowWidth originalWindowHeight firstFrame
      Nothing -> fail "Could not find first frame"
  startTime <- SDL.time
  writeChan channel Render
  State.evalStateT
    (do scale
        whileM $
          lift (readChan channel) >>= \case
            Stop ->
              lift cleanup >> use texture >>= lift . SDL.destroyTexture >>
              return False
            Resize windowWidth windowHeight -> do
              use texture >>= lift . SDL.destroyTexture
              newState <-
                use frame >>= lift . allocate renderer windowWidth windowHeight
              State.put newState
              scale
              updateRenderer renderer
              return True
            Next ->
              lift reader >>= \case
                Nothing -> return True
                Just (frame', time) ->
                  frame .= frame' >> scale >> lift SDL.time >>= \currentTime ->
                    lift
                      (forkIO
                         (when
                            (currentTime < startTime + time)
                            (threadDelay $
                             floor $
                             (* 1000000) $ startTime + time - currentTime) >>
                          writeChan channel Render)) >>
                    return True
            Render ->
              updateRenderer renderer >> lift (writeChan channel Next) >>
              return True)
    initialState

updateRenderer :: SDL.Renderer -> StateT State IO ()
updateRenderer renderer =
  SDL.clear renderer >> use texture >>= \t ->
    use rectangle >>= SDL.copy renderer t Nothing . Just >> SDL.present renderer

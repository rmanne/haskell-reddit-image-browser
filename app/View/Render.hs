{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module View.Render
  ( newThread
  , Action(Stop, Resize)
  ) where

import qualified Codec.FFmpeg as FFmpeg
import qualified Codec.FFmpeg.Decode as FFmpeg
import qualified Codec.Picture.Types as Pic
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Lens
  ( (%=)
  , (%~)
  , (&)
  , (+=)
  , (-=)
  , (.=)
  , (.~)
  , (^.)
  , _2
  , makeLenses
  , use
  )
import Control.Monad.Except (when)
import Control.Monad.Extra (whileM)
import Control.Monad.ST (RealWorld)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad.Trans.State.Strict (StateT)
import qualified Data.Vector.Storable as Vector
import Data.Vector.Storable.ByteString (vectorToByteString)
import qualified Data.Vector.Storable.Mutable as Vector
import Foreign.C.Types (CInt)
import qualified SDL
import qualified SDL.Video.Renderer as SDL

data Action
  = Stop
  | Resize
      { windowWidth :: CInt
      , windowHeight :: CInt
      }
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
  let scaleWidth :: Double
      scaleWidth = fromIntegral imageWidth / fromIntegral windowWidth
      scaleHeight :: Double
      scaleHeight = fromIntegral imageHeight / fromIntegral windowHeight
      scale :: Double
      scale = max scaleWidth scaleHeight
      (scaledWidth, scaledHeight) =
        ( floor (fromIntegral imageWidth / scale)
        , floor (fromIntegral imageHeight / scale))
      (horizontalOffset, verticalOffset) =
        if scaleWidth < scaleHeight
          then (floor (fromIntegral (windowWidth - scaledWidth) / 2), 0)
          else (0, floor (fromIntegral (windowHeight - scaledHeight) / 2))
   in SDL.Rectangle
        (SDL.P (SDL.V2 horizontalOffset verticalOffset))
        (SDL.V2 scaledWidth scaledHeight)

-- TODO: scaling with something like bilinear scaling per-image is hella slow
-- look into libswscale (ffmpeg) and generally heavier bindings to ffmpeg
-- Additionally, might have to perform the scaling on a separate thread
-- http://hackage.haskell.org/package/hs-ffmpeg-0.3.4/src/src/Media/FFMpeg/SWScale.hsc
-- https://trac.ffmpeg.org/wiki/Using%20libav*
scale :: StateT State IO ()
scale =
  use texture >>= \texture ->
    use frame >>= \frame ->
      SDL.updateTexture
        texture
        Nothing
        (vectorToByteString (Pic.imageData frame))
        (fromIntegral $ Pic.imageWidth frame * 3) >>
      return ()

allocate :: SDL.Renderer -> CInt -> CInt -> Pic.Image Pixel -> IO State
allocate renderer windowWidth windowHeight frame = do
  let rectangle@(SDL.Rectangle _ (SDL.V2 scaledWidth scaledHeight)) =
        rectangleFor frame windowWidth windowHeight
  texture <-
    SDL.createTexture
      renderer
      SDL.RGB24
      SDL.TextureAccessStreaming
      (SDL.V2
         (fromIntegral $ Pic.imageWidth frame)
         (fromIntegral $ Pic.imageHeight frame))
  return State {_rectangle = rectangle, _texture = texture, _frame = frame}

newThread :: SDL.Renderer -> FilePath -> CInt -> CInt -> Chan Action -> IO ()
newThread renderer file originalWindowWidth originalWindowHeight channel = do
  (reader, cleanup) <- FFmpeg.imageReaderTime (FFmpeg.File file)
  initialState <-
    reader >>= \(Just (frame, _)) ->
      allocate renderer originalWindowWidth originalWindowHeight frame
  startTime <- SDL.time
  writeChan channel Render
  State.evalStateT
    (do scale
        whileM $
          lift (readChan channel) >>= \case
            Stop -> use texture >>= lift . SDL.destroyTexture >> return False
            Resize {..} -> do
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
                  frame .= frame' >>
                  scale >> lift SDL.time >>= \currentTime ->
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
  SDL.clear renderer >> use texture >>= \texture ->
    use rectangle >>= SDL.copy renderer texture Nothing . Just >>
    SDL.present renderer

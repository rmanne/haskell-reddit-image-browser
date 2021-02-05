{-# LANGUAGE TemplateHaskell #-}

module View.Render
  ( newThread
  , Action(Stop, Resize)
  ) where

import qualified Codec.FFmpeg as FFmpeg
import qualified Codec.Picture.Types as Pic
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Lens ((.=), _1, _2, _3, _4, makeLenses, use, view)
import Control.Monad (void, when)
import Control.Monad.Extra (whileM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.State.Strict (MonadState, evalStateT, put)
import Control.Monad.Writer (MonadWriter, execWriterT, tell)
import Data.Monoid (All(All), getAll)
import Data.Vector.Storable.ByteString (vectorToByteString)
import Foreign.C.Types (CInt)
import qualified SDL

data Action
  = Stop
  | Resize (SDL.V2 CInt)
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

imageDimensionsFor :: Num b => Pic.Image a -> SDL.V2 b
imageDimensionsFor Pic.Image {..} =
  fromIntegral <$> SDL.V2 imageWidth imageHeight

rectangleFor :: Pic.Image Pixel -> SDL.V2 CInt -> SDL.Rectangle CInt
rectangleFor image windowDimensions =
  let imageDimensions :: SDL.V2 Double
      imageDimensions = imageDimensionsFor image
      maxScale :: Double
      maxScale = maximum (imageDimensions / (fromIntegral <$> windowDimensions))
      dimensions :: SDL.V2 CInt
      dimensions = floor . (/ maxScale) <$> imageDimensions
   in SDL.Rectangle
        (SDL.P ((`div` 2) <$> (windowDimensions - dimensions)))
        dimensions

updateTexture :: (MonadState State m, MonadIO m) => SDL.Renderer -> m ()
updateTexture renderer =
  use texture >>= \t ->
    use frame >>= \f ->
      SDL.updateTexture
        t
        Nothing
        (vectorToByteString (Pic.imageData f))
        (fromIntegral $ Pic.imageWidth f * 3) >>
      SDL.clear renderer >>
      use rectangle >>=
      SDL.copy renderer t Nothing . Just >>
      return ()

allocate ::
     MonadIO m => SDL.Renderer -> SDL.V2 CInt -> Pic.Image Pixel -> m State
allocate renderer windowDimensions f =
  State (rectangleFor f windowDimensions) <$>
  SDL.createTexture
    renderer
    SDL.RGB24
    SDL.TextureAccessStreaming
    (imageDimensionsFor f) <*>
  pure f

newThread :: SDL.Renderer -> FilePath -> SDL.V2 CInt -> Chan Action -> IO ()
newThread renderer file originalWindowDimensions channel = do
  (reader, cleanup) <- FFmpeg.imageReaderTime (FFmpeg.File file)
  initialState <-
    reader >>= \case
      Just (firstFrame, _) ->
        allocate renderer originalWindowDimensions firstFrame
      Nothing -> fail "Could not find first frame"
  startTime <- SDL.time
  writeChan channel Render
  evalStateT
    (do updateTexture renderer
        whileM $
          liftIO (readChan channel) >>= \action ->
            getAll <$>
            execWriterT
              (runReaderT
                 (onAction action)
                 (renderer, reader, startTime, channel)))
    initialState
  cleanup

onAction ::
     ( MonadReader ( SDL.Renderer
                   , IO (Maybe (Pic.Image Pixel, Double))
                   , Double
                   , Chan Action) m
     , MonadIO m
     , MonadState State m
     , MonadWriter All m
     )
  => Action
  -> m ()
onAction Stop = use texture >>= SDL.destroyTexture >> tell (All False)
onAction (Resize windowDimensions) =
  view _1 >>= \renderer ->
    use texture >>= SDL.destroyTexture >> use frame >>=
    allocate renderer windowDimensions >>=
    put >>
    updateTexture renderer >>
    SDL.present renderer
onAction Next =
  view _2 >>= liftIO >>= \case
    Nothing -> return ()
    Just (frame', time) ->
      frame .= frame' >> view _1 >>= updateTexture >> SDL.time >>= \currentTime ->
        view _3 >>= \startTime ->
          view _4 >>=
          after (startTime + time - currentTime) . liftIO . (`writeChan` Render)
onAction Render =
  view _1 >>= SDL.present >> view _4 >>= liftIO . (`writeChan` Next)

-- | Run a command after the specified number of seconds
after :: MonadIO m => Double -> IO () -> m ()
after seconds = void . liftIO . forkIO . (delay >>)
  where
    delay :: IO ()
    delay = when (seconds > 0) $ threadDelay $ floor $ seconds * 1000000

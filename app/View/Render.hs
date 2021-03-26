{-# LANGUAGE TemplateHaskell #-}

module View.Render
  ( newThread
  , Action(Stop, Resize)
  ) where

import qualified Codec.FFmpeg as FFmpeg
import qualified Codec.Picture.Types as Pic
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Lens ((.=), makeLenses, use, view)
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

data Environment =
  Environment
    { _renderer :: SDL.Renderer
    , _channel :: Chan Action
    , _reader :: IO (Maybe (Pic.Image Pixel, Double))
    , _startTime :: Double
    }

$(makeLenses ''Environment)

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

updateTexture ::
     (MonadState State m, MonadReader Environment m, MonadIO m) => m ()
updateTexture =
  use texture >>= \t ->
    use frame >>= \f ->
      SDL.updateTexture
        t
        Nothing
        (vectorToByteString (Pic.imageData f))
        (fromIntegral $ Pic.imageWidth f * 3) >>
      view renderer >>= \r ->
        SDL.clear r >> use rectangle >>= SDL.copy r t Nothing . Just >>
        return ()

allocate ::
     (MonadReader Environment m, MonadIO m)
  => SDL.V2 CInt
  -> Pic.Image Pixel
  -> m State
allocate windowDimensions f =
  view renderer >>= \r ->
    State (rectangleFor f windowDimensions) <$>
    SDL.createTexture
      r
      SDL.RGB24
      SDL.TextureAccessStreaming
      (imageDimensionsFor f) <*>
    pure f

newThread :: SDL.Renderer -> FilePath -> SDL.V2 CInt -> Chan Action -> IO ()
newThread renderer' file originalWindowDimensions channel' = do
  (reader', cleanup) <- FFmpeg.imageReaderTime (FFmpeg.File file)
  writeChan channel' Render
  reader' >>= \case
    Just (firstFrame, _) ->
      (Environment renderer' channel' reader' <$> SDL.time) >>=
      runReaderT
        (allocate originalWindowDimensions firstFrame >>=
         evalStateT actionProcessor)
    Nothing -> cleanup >> fail "Could not find first frame"
  cleanup

actionProcessor ::
     (MonadReader Environment m, MonadState State m, MonadIO m) => m ()
actionProcessor =
  updateTexture >>
  whileM
    (view channel >>= liftIO . readChan >>= \action ->
       getAll <$> execWriterT (onAction action))

onAction ::
     ( MonadReader Environment m
     , MonadIO m
     , MonadState State m
     , MonadWriter All m
     )
  => Action
  -> m ()
onAction Stop = use texture >>= SDL.destroyTexture >> tell (All False)
onAction (Resize windowDimensions) =
  use texture >>= SDL.destroyTexture >> use frame >>= allocate windowDimensions >>=
  put >>
  updateTexture >>
  view renderer >>=
  SDL.present
onAction Next =
  view reader >>= liftIO >>= \case
    Nothing -> return ()
    Just (frame', time) ->
      frame .= frame' >> updateTexture >> SDL.time >>= \currentTime ->
        view startTime >>= \st ->
          view channel >>=
          after (st + time - currentTime) . liftIO . (`writeChan` Render)
onAction Render =
  view renderer >>= SDL.present >> view channel >>= liftIO . (`writeChan` Next)

-- | Run a command after the specified number of seconds
after :: MonadIO m => Double -> IO () -> m ()
after seconds = void . liftIO . forkIO . (delay >>)
  where
    delay :: IO ()
    delay = when (seconds > 0) $ threadDelay $ floor $ seconds * 1000000

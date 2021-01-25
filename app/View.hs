module View
  ( view
  ) where

import qualified Codec.FFmpeg as FFmpeg
import qualified Codec.FFmpeg.Common as FFmpegCommon
import qualified Codec.FFmpeg.Decode as FFmpeg
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan, writeChan)
import Control.Monad (join)
import Control.Monad.Except (runExceptT, when)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State.Strict as State
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackCStringFinalizer)
import Data.Default (Default(def))
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (utctDay)
import EventHandler
import Foreign.C.Types (CInt)
import Foreign.Ptr (castPtr)
import qualified Reddit.Types.Listing as R
import qualified Reddit.Types.Post as R
import qualified SDL
import Types

-- TODO: Audio
-- https://hackage.haskell.org/package/sdl2-2.5.3.0/docs/SDL-Audio.html
-- https://github.com/fosterseth/sdl2_video_player/blob/master/vidserv.c
-- https://github.com/acowley/ffmpeg-light/tree/audio
-- https://github.com/acowley/ffmpeg-light/issues/21
-- https://github.com/acowley/ffmpeg-light/compare/audio#diff-cbdc928a28fd3b49d906aab9cdb228bc31f3d3017fcbb6b411082f6a3feb6fa7
-- TODO: Render text
-- https://hackage.haskell.org/package/sdl2-ttf-2.1.1/docs/SDL-Font.html
data ViewResources =
  ViewResources
    { modelChannel :: Chan Command
    , window :: SDL.Window
    , renderer :: SDL.Renderer
    , renderingContextBuilder :: CInt -> CInt -> RenderingContext
    , renderingContext :: RenderingContext
    , nextFrame :: ViewIO ()
    , currentFrame :: ViewIO ()
    , cleanup :: IO ()
    , windowWidth :: CInt
    , windowHeight :: CInt
    , quit :: Bool
    }

type ViewIO = State.StateT ViewResources IO

-- | This should be run in the main thread. This function will not return until the view is closed.
view :: Chan Command -> IORef (Maybe View) -> IO ()
view controllerChannel viewRef = do
  FFmpeg.initFFmpeg
  SDL.initializeAll
  window <-
    SDL.createWindow "Loading..." SDL.defaultWindow {SDL.windowResizable = True}
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
  SDL.clear renderer
  let viewResources =
        ViewResources
          { modelChannel = controllerChannel
          , window = window
          , renderer = renderer
          , renderingContextBuilder = \_ _ -> def
          , renderingContext = def
          , nextFrame = return ()
          , currentFrame = return ()
          , cleanup = return ()
          , windowHeight = 100
          , windowWidth = 100
          , quit = False
          }
  viewResources' <- State.execStateT (viewLoop viewRef) viewResources
  cleanup viewResources'
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

viewToTitleFile :: View -> (String, Maybe FilePath)
viewToTitleFile (Deleted (R.PostID post), _) =
  ("[Deleted] " <> show post, Nothing)
viewToTitleFile (Failed (R.PostID post), currentIndex) =
  ("[Failed] " <> show post, Nothing)
viewToTitleFile (Submitted post, currentIndex) =
  ( "[Downloading] " <>
    (let (_, month, day) = toGregorian $ utctDay (R.created post)
      in show month <> "/" <> show day) <>
    " [score=" <> show (R.score post) <> "] " <> Text.unpack (R.title post)
  , Nothing)
viewToTitleFile (Downloaded post files, currentIndex) =
  let currentIndex' = currentIndex `mod` length files
   in ( (let (_, month, day) = toGregorian $ utctDay (R.created post)
          in show month <> "/" <> show day) <>
        " [score=" <>
        show (R.score post) <>
        "] [" <>
        show currentIndex' <>
        "/" <>
        show (length files) <>
        "] [" <>
        (let R.PostID postId = R.postID post
          in Text.unpack postId) <>
        "] " <> Text.unpack (R.title post)
      , Just (files !! currentIndex'))

data RenderingContext =
  RenderingContext
    { destinationRectangle :: SDL.Rectangle CInt
    }

instance Default RenderingContext where
  def = RenderingContext $ SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 0 0)

renderingGenerator ::
     SDL.Renderer
  -> FilePath
  -> IO (CInt -> CInt -> RenderingContext, ViewIO (), ViewIO (), IO ())
renderingGenerator renderer file = do
  (reader, cleanup) <-
    runExceptT (FFmpeg.frameReaderTime FFmpeg.avPixFmtRgb24 (FFmpeg.File file)) >>= \case
      Right v -> return v
      Left msg -> fail msg
  (firstFrame, firstFrameTime) <-
    reader >>= \case
      Nothing -> fail "First frame is empty"
      Just v -> return v
  Just imageBufferSize <- FFmpegCommon.frameBufferSize firstFrame
  Just imageLineSize <- FFmpegCommon.frameLineSize firstFrame
  unsafeImageBuffer <- FFmpegCommon.av_malloc (fromIntegral imageBufferSize)
  imageBuffer <-
    unsafePackCStringFinalizer
      (castPtr unsafeImageBuffer)
      (fromIntegral imageBufferSize)
      (FFmpegCommon.av_free unsafeImageBuffer)
  let textureWidth = imageLineSize `div` 3 -- 3 because Rgb24 format
      textureHeight = imageBufferSize `div` imageLineSize
  texture <-
    SDL.createTexture
      renderer
      SDL.RGB24
      SDL.TextureAccessStreaming
      (SDL.V2 textureWidth textureHeight)
  startTime <- SDL.time
  let currentFrame =
        State.gets renderingContext >>= \RenderingContext {destinationRectangle} -> do
          SDL.clear renderer
          SDL.copy renderer texture Nothing (Just destinationRectangle)
          SDL.present renderer
      updateTexture frame = do
        Just _ <-
          FFmpegCommon.frameCopyToBuffer frame (castPtr unsafeImageBuffer)
        _ <- SDL.updateTexture texture Nothing imageBuffer imageLineSize
        return ()
      nextFrame =
        lift
          (reader >>= \case
             Nothing -> threadDelay 30000
             Just (frame, time) -> do
               currentTime <- SDL.time
               when (currentTime < startTime + time) $
                 threadDelay $
                 floor $ (* 1000000) $ startTime + time - currentTime
               updateTexture frame) >>
        currentFrame
      rectangleFor windowWidth windowHeight =
        let scaleWidth :: Double
            scaleWidth = fromIntegral textureWidth / fromIntegral windowWidth
            scaleHeight :: Double
            scaleHeight = fromIntegral textureHeight / fromIntegral windowHeight
            scale :: Double
            scale = max scaleWidth scaleHeight
            (scaledWidth, scaledHeight) =
              ( floor (fromIntegral textureWidth / scale)
              , floor (fromIntegral textureHeight / scale))
            (horizontalOffset, verticalOffset) =
              if scaleWidth < scaleHeight
                then (floor (fromIntegral (windowWidth - scaledWidth) / 2), 0)
                else (0, floor (fromIntegral (windowHeight - scaledHeight) / 2))
         in SDL.Rectangle
              (SDL.P (SDL.V2 horizontalOffset verticalOffset))
              (SDL.V2 scaledWidth scaledHeight)
  updateTexture firstFrame
  return
    ( \width height -> RenderingContext (rectangleFor width height)
    , nextFrame
    , currentFrame
    , SDL.destroyTexture texture >> cleanup)

switchFile :: Maybe FilePath -> ViewIO ()
switchFile Nothing = do
  renderer <- State.gets renderer
  lift $ SDL.clear renderer
  lift $ SDL.present renderer
  State.modify $ \viewResources ->
    viewResources
      { renderingContextBuilder = \_ _ -> def
      , renderingContext = def
      , nextFrame = return ()
      , currentFrame = return ()
      , cleanup = return ()
      }
switchFile (Just file) = do
  renderer <- State.gets renderer
  (renderingContextBuilder, nextFrame, currentFrame, cleanup) <-
    lift $ renderingGenerator renderer file
  renderingContext <-
    renderingContextBuilder <$> State.gets windowWidth <*>
    State.gets windowHeight
  State.modify $ \viewResources ->
    viewResources
      { renderingContextBuilder = renderingContextBuilder
      , renderingContext = renderingContext
      , nextFrame = nextFrame
      , currentFrame = currentFrame
      , cleanup = cleanup
      }
  currentFrame

viewLoop :: IORef (Maybe View) -> ViewIO ()
viewLoop viewRef =
  SDL.mapEvents processEventT >> State.gets quit >>= \case
    True -> return ()
    False ->
      lift (atomicModifyIORef viewRef (Nothing, )) >>= \case
        Nothing -> join (State.gets nextFrame) >> viewLoop viewRef
        Just view -> do
          State.gets cleanup >>= lift
          let (title, file) = viewToTitleFile view
          lift $ print (title, file)
          State.gets window >>= \window ->
            SDL.windowTitle window SDL.$= Text.pack title
          switchFile file
          viewLoop viewRef
  where
    processEventT :: SDL.Event -> ViewIO ()
    processEventT event =
      State.gets modelChannel >>= \modelChannel ->
        lift (processEvent modelChannel event) >>= \case
          Just Quit ->
            State.modify $ \viewResources -> viewResources {quit = True}
          Just (ResizeWindow width height) ->
            State.gets renderingContextBuilder >>= \builder ->
              State.modify
                (\viewResources ->
                   viewResources
                     { windowWidth = width
                     , windowHeight = height
                     , renderingContext = builder width height
                     }) >>
              join (State.gets currentFrame)
          Nothing -> return ()

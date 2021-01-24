module View
  ( view
  ) where

import qualified Codec.FFmpeg as FFmpeg
import qualified Codec.FFmpeg.Common as FFmpegCommon
import qualified Codec.FFmpeg.Decode as FFmpeg
import Control.Monad.Except(runExceptT, when)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackCStringFinalizer)
import Data.Text (Text)
import Data.IORef(atomicModifyIORef, IORef)
import Foreign.C.Types(CInt)
import Foreign.Ptr(castPtr)
import Control.Concurrent.Chan (Chan, writeChan)
import qualified Data.Text as Text
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (utctDay)
import qualified Reddit.Types.Listing as R
import qualified Reddit.Types.Post as R
import qualified SDL
import Types

data ViewResources =
  ViewResources
    { window :: SDL.Window
    , renderer :: SDL.Renderer
    , nextFrame :: IO ()
    , newNextFrame :: CInt -> CInt -> IO ()
    , cleanup :: IO ()
    , windowWidth :: CInt
    , windowHeight :: CInt
    , quitting :: Bool
    }

-- | This should be run in the main thread. This function will not return until the view is closed.
view :: Chan Command -> IORef (Maybe View) -> IO ()
view controllerChannel viewRef = do
  FFmpeg.initFFmpeg
  SDL.initializeAll
  viewResources <- createViewResources
  viewLoop controllerChannel viewRef viewResources
  destroyViewResources viewResources
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
        "] [" <> show (R.postID post) <> "] " <> Text.unpack (R.title post)
      , Just (files !! currentIndex'))

newNextFrameGenerator ::
     CInt
  -> CInt
  -> SDL.Renderer
  -> FilePath
  -> IO (CInt -> CInt -> IO (), IO ())
newNextFrameGenerator currentWindowWidth currentWindowHeight renderer file = do
  (reader, cleanup) <-
    runExceptT (FFmpeg.frameReaderTime FFmpeg.avPixFmtRgb24 (FFmpeg.File file)) >>= \case
      Right v -> return v
      Left msg -> fail msg
  (firstFrame, firstFrameTime) <- reader >>= \case
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
  let renderFrame (frame, time) destinationRectangle = do
        currentTime <- SDL.time
        when (currentTime < startTime + time) $
          SDL.delay $ sec2msec $ startTime + time - currentTime
        Just _ <-
          FFmpegCommon.frameCopyToBuffer frame (castPtr unsafeImageBuffer)
        _ <- SDL.updateTexture texture Nothing imageBuffer imageLineSize
        SDL.clear renderer
        SDL.copy renderer texture Nothing (Just destinationRectangle)
        SDL.present renderer
      nextFrame destinationRectangle =
        reader >>= \case
          Nothing -> SDL.delay 100 >> return ()
          Just (frame, time) -> renderFrame (frame, time) destinationRectangle
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
      newNextFrame windowWidth windowHeight =
        nextFrame (rectangleFor windowWidth windowHeight)
  renderFrame
    (firstFrame, firstFrameTime)
    (rectangleFor currentWindowWidth currentWindowHeight)
  return (newNextFrame, SDL.destroyTexture texture >> cleanup)

viewLoopNoEvents :: Chan Command -> IORef (Maybe View) -> ViewResources -> IO ()
viewLoopNoEvents controllerChannel viewRef viewResources =
  atomicModifyIORef viewRef (Nothing, ) >>= \case
    Just view -> do
      _ <- cleanup viewResources
      let (title, file) = viewToTitleFile view
      print (title, file)
      SDL.windowTitle (window viewResources) SDL.$= Text.pack title
      case file of
        Nothing -> do
          SDL.clear (renderer viewResources)
          SDL.present (renderer viewResources)
          viewLoop
            controllerChannel
            viewRef
            viewResources
              { newNextFrame = \_ _ -> return ()
              , nextFrame = return ()
              , cleanup = return ()
              }
        Just file' -> do
          (newNextFrame', cleanup') <-
            newNextFrameGenerator
              (windowWidth viewResources)
              (windowHeight viewResources)
              (renderer viewResources)
              file'
          viewLoop
            controllerChannel
            viewRef
            viewResources
              { newNextFrame = newNextFrame'
              , nextFrame =
                  newNextFrame'
                    (windowWidth viewResources)
                    (windowHeight viewResources)
              , cleanup = cleanup'
              }
    Nothing ->
      nextFrame viewResources >>
      viewLoop controllerChannel viewRef viewResources

viewLoop :: Chan Command -> IORef (Maybe View) -> ViewResources -> IO ()
viewLoop _ _ ViewResources {quitting = True} = return ()
viewLoop controllerChannel viewRef viewResources =
  SDL.pollEvents >>= \case
    [] -> viewLoopNoEvents controllerChannel viewRef viewResources
    events ->
      eventLoop controllerChannel viewResources events >>=
      viewLoop controllerChannel viewRef

eventLoop :: Chan Command -> ViewResources -> [SDL.Event] -> IO ViewResources
eventLoop _ viewResources [] = return viewResources
eventLoop controllerChannel viewResources (SDL.Event {SDL.eventPayload = SDL.WindowSizeChangedEvent SDL.WindowSizeChangedEventData {SDL.windowSizeChangedEventSize = SDL.V2 width height}}:remainingEvents) =
  eventLoop
    controllerChannel
    viewResources
      { windowWidth = fromIntegral width
      , windowHeight = fromIntegral height
      , nextFrame =
          newNextFrame viewResources
            (fromIntegral width)
            (fromIntegral height)
      }
    remainingEvents
eventLoop controllerChannel viewResources (SDL.Event {SDL.eventPayload = SDL.QuitEvent}:remainingEvents) =
  print "QuitEvent" >> return viewResources {quitting = True}
eventLoop controllerChannel viewResources (SDL.Event {SDL.eventPayload = SDL.KeyboardEvent SDL.KeyboardEventData { SDL.keyboardEventKeysym = keysym
                                                                                                                 , SDL.keyboardEventKeyMotion = SDL.Released
                                                                                                                 }}:remainingEvents) =
  case SDL.keysymModifier keysym of
    SDL.KeyModifier { SDL.keyModifierLeftShift = False
                    , SDL.keyModifierRightShift = False
                    , SDL.keyModifierLeftCtrl = False
                    , SDL.keyModifierRightCtrl = False
                    , SDL.keyModifierLeftAlt = False
                    , SDL.keyModifierRightAlt = False
                    , SDL.keyModifierLeftGUI = False
                    , SDL.keyModifierRightGUI = False
                    , SDL.keyModifierNumLock = False
                    , SDL.keyModifierCapsLock = False
                    , SDL.keyModifierAltGr = False
                    } ->
      case SDL.keysymKeycode keysym of
        SDL.KeycodeQ -> return viewResources {quitting = True}
        SDL.KeycodeJ -> nextLoop Next
        SDL.KeycodeK -> nextLoop Prev
        SDL.KeycodeS -> nextLoop Toggle
        SDL.KeycodeD -> nextLoop Remove
        SDL.KeycodeW -> nextLoop Save
        SDL.KeycodeV -> nextLoop ToggleDeleted
        SDL.KeycodeR -> nextLoop Refresh
        SDL.KeycodeG -> nextLoop Front
        SDL.KeycodeC -> nextLoop Commit
        SDL.KeycodeF -> nextLoop FindFailed
        SDL.KeycodeA -> nextLoop Status
        SDL.KeycodeL -> nextLoop NextImage
        SDL.KeycodeH -> nextLoop PrevImage
        SDL.Keycode0 -> nextLoop (Multi 0)
        SDL.Keycode1 -> nextLoop (Multi 1)
        SDL.Keycode2 -> nextLoop (Multi 2)
        SDL.Keycode3 -> nextLoop (Multi 3)
        SDL.Keycode4 -> nextLoop (Multi 4)
        SDL.Keycode5 -> nextLoop (Multi 5)
        SDL.Keycode6 -> nextLoop (Multi 6)
        SDL.Keycode7 -> nextLoop (Multi 7)
        SDL.Keycode8 -> nextLoop (Multi 8)
        SDL.Keycode9 -> nextLoop (Multi 9)
        _ -> eventLoop controllerChannel viewResources remainingEvents
    SDL.KeyModifier { SDL.keyModifierLeftShift = lshift
                    , SDL.keyModifierRightShift = rshift
                    , SDL.keyModifierLeftCtrl = False
                    , SDL.keyModifierRightCtrl = False
                    , SDL.keyModifierLeftAlt = False
                    , SDL.keyModifierRightAlt = False
                    , SDL.keyModifierLeftGUI = False
                    , SDL.keyModifierRightGUI = False
                    , SDL.keyModifierNumLock = False
                    , SDL.keyModifierCapsLock = False
                    , SDL.keyModifierAltGr = False
                    }
      | lshift || rshift ->
        case SDL.keysymKeycode keysym of
          SDL.KeycodeQ -> return viewResources {quitting = True}
          SDL.KeycodeG -> nextLoop Back
          _ -> eventLoop controllerChannel viewResources remainingEvents
    _ -> eventLoop controllerChannel viewResources remainingEvents
  where
    nextLoop command =
      writeChan controllerChannel command >>
      eventLoop controllerChannel viewResources remainingEvents
eventLoop controllerChannel viewResources (_:remainingEvents) =
  eventLoop controllerChannel viewResources remainingEvents

-- Create window, renderer and texture.
createViewResources :: IO ViewResources
createViewResources = do
  window <- createWindow
  renderer <- createRenderer window
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
  SDL.clear renderer
  return
    ViewResources
      { window = window
      , renderer = renderer
      , nextFrame = return ()
      , newNextFrame = \_ _ -> return ()
      , cleanup = return ()
      , windowHeight = 100
      , windowWidth = 100
      , quitting = False
      }
  where
    createWindow =
      SDL.createWindow
        "Loading..."
        SDL.defaultWindow {SDL.windowResizable = True}
    -- Create renderer using driver from config.
    createRenderer window = SDL.createRenderer window (-1) SDL.defaultRenderer

destroyViewResources :: ViewResources -> IO ()
destroyViewResources ViewResources {cleanup, window, renderer} =
  cleanup >> SDL.destroyRenderer renderer >> SDL.destroyWindow window

-- Convert floating point second to millisecond.
sec2msec :: (RealFrac a, Integral b) => a -> b
sec2msec = floor . (* 1000)

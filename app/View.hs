{-# LANGUAGE TemplateHaskell #-}

module View
  ( view
  ) where

import qualified Codec.FFmpeg as FFmpeg
import qualified Codec.FFmpeg.Common as FFmpegCommon
import qualified Codec.FFmpeg.Decode as FFmpeg
import Control.Concurrent
  ( MVar
  , forkOn
  , newEmptyMVar
  , putMVar
  , takeMVar
  , threadDelay
  )
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
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
import Control.Monad (forever, join)
import Control.Monad.Except (runExceptT, when)
import Control.Monad.Extra (unlessM, whileM)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad.Trans.State.Strict (StateT)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackCStringFinalizer)
import Data.Default (Default(def))
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (utctDay)
import qualified EventHandler
import Foreign.C.Types (CInt)
import Foreign.Ptr (castPtr)
import qualified Reddit.Types.Listing as R
import qualified Reddit.Types.Post as R
import qualified SDL
import Types
import View.Render (newThread)
import qualified View.Render
import View.Types

-- TODO: Audio
-- https://hackage.haskell.org/package/sdl2-2.5.3.0/docs/SDL-Audio.html
-- https://github.com/fosterseth/sdl2_video_player/blob/master/vidserv.c
-- https://github.com/acowley/ffmpeg-light/tree/audio
-- https://github.com/acowley/ffmpeg-light/issues/21
-- https://github.com/acowley/ffmpeg-light/compare/audio#diff-cbdc928a28fd3b49d906aab9cdb228bc31f3d3017fcbb6b411082f6a3feb6fa7
-- TODO: Render text
-- https://hackage.haskell.org/package/sdl2-ttf-2.1.1/docs/SDL-Font.html
data State =
  State
    { _modelChannel :: Chan Command
    , _window :: SDL.Window
    , _renderer :: SDL.Renderer
    , _windowWidth :: CInt
    , _windowHeight :: CInt
    , _renderChannel :: Maybe (Chan View.Render.Action, MVar ())
    , _quit :: Bool
    }

$(makeLenses ''State)

-- | This should be run in the main thread. This function will not return until the view is closed.
view :: Chan Command -> Chan Action -> IO ()
view controllerChannel viewChannel = do
  FFmpeg.initFFmpeg
  SDL.initializeAll
  window <-
    SDL.createWindow "Loading..." SDL.defaultWindow {SDL.windowResizable = True}
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
  SDL.clear renderer
  let viewResources =
        State
          { _modelChannel = controllerChannel
          , _window = window
          , _renderer = renderer
          , _windowHeight = 100
          , _windowWidth = 100
          , _renderChannel = Nothing
          , _quit = False
          }
  forkOn 0 (eventLoop controllerChannel viewChannel)
  State.execStateT (viewLoop viewChannel) viewResources
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

eventLoop :: Chan Command -> Chan Action -> IO ()
eventLoop modelChannel viewChannel =
  whileM $
  SDL.waitEvent >>= EventHandler.processEvent modelChannel >>= \case
    Just EventHandler.Quit -> writeChan viewChannel Quit >> return False
    Just (EventHandler.ResizeWindow width height) ->
      writeChan viewChannel (Resize width height) >> return True
    Nothing -> return True

switchFile :: Maybe FilePath -> StateT State IO ()
switchFile Nothing = do
  renderer <- use renderer
  lift $ SDL.clear renderer
  lift $ SDL.present renderer
switchFile (Just file) = do
  renderer <- use renderer
  channel <- lift newChan
  handle <- lift newEmptyMVar
  renderChannel .= Just (channel, handle)
  width <- use windowWidth
  height <- use windowHeight
  lift
    (forkOn
       0
       (newThread renderer file width height channel >> putMVar handle ()))
  return ()

endRenderingThread :: StateT State IO ()
endRenderingThread =
  use renderChannel >>= \case
    Nothing -> return ()
    Just (channel, mvar) ->
      lift (writeChan channel View.Render.Stop) >> lift (takeMVar mvar) >>
      renderChannel .= Nothing

viewLoop :: Chan Action -> StateT State IO ()
viewLoop actionChannel =
  whileM $
  lift (readChan actionChannel) >>= \case
    Quit -> do
      endRenderingThread
      return False
    Resize width height ->
      use renderChannel >>= \case
        Nothing -> return True
        Just (channel, _) ->
          lift (writeChan channel (View.Render.Resize width height)) >>
          windowWidth .= width >>
          windowHeight .= height >>
          return True
    Update title file -> do
      endRenderingThread
      use window >>= \window -> SDL.windowTitle window SDL.$= Text.pack title
      switchFile file
      return True

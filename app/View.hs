{-# LANGUAGE TemplateHaskell #-}

module View
  ( view
  ) where

import qualified Codec.FFmpeg as FFmpeg
import Control.Concurrent (MVar, forkOn, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Lens ((.=), makeLenses, use, uses)
import Control.Monad.Extra (whileM)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad.Trans.State.Strict (StateT)
import qualified Data.Text as Text
import qualified EventHandler
import Foreign.C.Types (CInt)
import qualified SDL
import Types (Command)
import View.Render (newThread)
import qualified View.Render
import View.Types (Action(Quit, Resize, Update))

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
    { _window :: SDL.Window
    , _renderer :: SDL.Renderer
    , _windowWidth :: CInt
    , _windowHeight :: CInt
    , _renderChannel :: Maybe (Chan View.Render.Action, MVar ())
    }

$(makeLenses ''State)

-- | This should be run in the main thread. This function will not return until the view is closed.
view :: Chan Command -> Chan Action -> IO ()
view controllerChannel viewChannel = do
  FFmpeg.initFFmpeg
  SDL.initializeAll
  w <-
    SDL.createWindow "Loading..." SDL.defaultWindow {SDL.windowResizable = True}
  r <- SDL.createRenderer w (-1) SDL.defaultRenderer
  SDL.rendererDrawColor r SDL.$= SDL.V4 0 0 0 255
  SDL.clear r
  let viewResources =
        State
          { _window = w
          , _renderer = r
          , _windowHeight = 100
          , _windowWidth = 100
          , _renderChannel = Nothing
          }
  _ <- forkOn 0 (eventLoop controllerChannel viewChannel)
  _ <- State.execStateT (viewLoop viewChannel) viewResources
  SDL.destroyRenderer r
  SDL.destroyWindow w
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
switchFile Nothing =
  use renderer >>= \r -> lift (SDL.clear r) >> lift (SDL.present r)
switchFile (Just file) = do
  r <- use renderer
  channel <- lift newChan
  handle <- lift newEmptyMVar
  renderChannel .= Just (channel, handle)
  width <- use windowWidth
  height <- use windowHeight
  _ <-
    lift (forkOn 0 (newThread r file width height channel >> putMVar handle ()))
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
      uses window SDL.windowTitle >>= \titleVar ->
        titleVar SDL.$= Text.pack title
      switchFile file
      return True

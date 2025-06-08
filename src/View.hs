{-# LANGUAGE TemplateHaskell #-}

module View
  ( view,
  )
where

import qualified Codec.FFmpeg as FFmpeg
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, runInBoundThread, takeMVar, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (finally)
import Control.Lens (makeLenses, use, uses, (.=), (??))
import Control.Monad.Extra (forever, whenJustM, whileM, (>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (MonadState, evalStateT)
import qualified Data.Text as Text
import qualified EventHandler
import Foreign.C.Types (CInt)
import qualified SDL
import Types (Command)
import View.Render (newThread)
import qualified View.Render
import View.Types (Action (ProcessEvents, Quit, Resize, Update))

-- TODO: Audio
-- https://hackage.haskell.org/package/sdl2-2.5.3.0/docs/SDL-Audio.html
-- https://github.com/fosterseth/sdl2_video_player/blob/master/vidserv.c
-- https://github.com/acowley/ffmpeg-light/tree/audio
-- https://github.com/acowley/ffmpeg-light/issues/21
-- https://github.com/acowley/ffmpeg-light/compare/audio#diff-cbdc928a28fd3b49d906aab9cdb228bc31f3d3017fcbb6b411082f6a3feb6fa7
-- TODO: Render text
-- https://hackage.haskell.org/package/sdl2-ttf-2.1.1/docs/SDL-Font.html
data State = State
  { _window :: SDL.Window,
    _renderer :: SDL.Renderer,
    _windowDimensions :: SDL.V2 CInt,
    _renderChannel :: Maybe (Chan View.Render.Action, MVar ())
  }

$(makeLenses ''State)

-- | This should be run in the main thread. This function will not return until the view is closed.
view :: Chan Command -> Chan Action -> IO ()
view controllerChannel viewChannel = do
  FFmpeg.initFFmpeg
  SDL.initializeAll
  True <-
    SDL.setHintWithPriority
      SDL.NormalPriority
      SDL.HintRenderScaleQuality
      SDL.ScaleLinear
  w <-
    SDL.createWindow "Loading..." SDL.defaultWindow {SDL.windowResizable = True}
  r <- SDL.createRenderer w (-1) SDL.defaultRenderer
  SDL.rendererDrawColor r SDL.$= SDL.V4 0 0 0 255
  SDL.clear r
  wd <- SDL.get (SDL.windowSize w)
  let viewResources =
        State
          { _window = w,
            _renderer = r,
            _windowDimensions = wd,
            _renderChannel = Nothing
          }
  _ <- forkIO $ forever $ writeChan viewChannel ProcessEvents >> threadDelay 5000
  _ <- evalStateT (viewLoop viewChannel controllerChannel) viewResources
  SDL.destroyRenderer r
  SDL.destroyWindow w
  SDL.quit

switchFile :: (MonadState State m, MonadIO m) => Maybe FilePath -> m ()
switchFile Nothing = use renderer >>= sequence_ . ([SDL.clear, SDL.present] ??)
switchFile (Just file) = do
  r <- use renderer
  channel <- liftIO newChan
  handle <- liftIO newEmptyMVar
  renderChannel .= Just (channel, handle)
  dimensions <- use windowDimensions
  _ <-
    liftIO (runInBoundThread $ forkIO (newThread r file dimensions channel `finally` putMVar handle ()))
  return ()

endRenderingThread :: (MonadState State m, MonadIO m) => m ()
endRenderingThread =
  whenJustM (use renderChannel) $ \(channel, mvar) ->
    liftIO (writeChan channel View.Render.Stop) >> liftIO (takeMVar mvar)
      >> renderChannel .= Nothing

viewLoop :: (MonadState State m, MonadIO m) => Chan Action -> Chan Command -> m ()
viewLoop actionChannel modelChannel =
  whileM $
    liftIO (readChan actionChannel) >>= \case
      Quit -> False <$ endRenderingThread
      Resize dimensions ->
        windowDimensions .= dimensions
          >> whenJustM
            (use renderChannel)
            ( \(channel, _) ->
                liftIO (writeChan channel (View.Render.Resize dimensions))
            )
          >> return True
      Update title file ->
        True <$ do
          endRenderingThread
          uses window SDL.windowTitle >>= (SDL.$= Text.pack title)
          switchFile file
      ProcessEvents ->
        True
          <$ SDL.mapEvents
            ( liftIO . EventHandler.processEvent modelChannel
                >=> ( \case
                        Just action -> liftIO (writeChan actionChannel action)
                        Nothing -> return ()
                    )
            )

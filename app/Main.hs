{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Control.Concurrent
import Controller (controller)
import qualified Data.GI.Base
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Data.Text (pack)
import qualified Data.GI.Base.Signals as Signals
import qualified GI.GLib as GLib
import qualified GI.GObject
import qualified GI.Gdk as Gdk
import qualified GI.GdkX11
import qualified GI.Gst as Gst
import qualified GI.GstVideo
import qualified GI.Gtk as Gtk
import Model (model)
import System.Environment (getArgs)
import Types
import Data.GI.Base.ManagedPtr(castTo)
import qualified View
import Data.GI.Base.Overloading as O
import Debug.Trace

newtype GstElement =
  GstElement Gst.Element

--instance GI.GstVideo.IsVideoOverlay Gst.Element

instance O.HasParentTypes GstElement
type instance O.ParentTypes GstElement = '[GI.GstVideo.VideoOverlay]
--instance GI.GstVideo.IsVideoOverlay GstElement

--instance Data.GI.Base.Overloading.CheckForAncestorType GstElement GI.GstVideo.VideoOverlay (Data.GI.Base.Overloading.ParentTypes GstElement)

drawingThread :: Gtk.Window -> Gst.Element -> IORef (Maybe View) -> IO ()
drawingThread window display drawingRef =
  atomicModifyIORef drawingRef (\v -> (Nothing, v)) >>= \case
    Nothing -> return ()
    Just v' -> View.view window display v'

main :: IO ()
main = do
  [subr] <- getArgs
  --Just application <- Gtk.applicationNew Nothing []
  _ <- Gst.init Nothing
  _ <- Gtk.init Nothing
  builder <-
    Gtk.builderNewFromFile "/home/work/hs-reddit-image-browser/bare.glade"
  window <- getObject Gtk.Window builder "window"
  requestChannel <- newChan
  --Signals.on application #startup $ do
  do
                                         --window <- Gtk.applicationWindowNew application
                                         --window' <- Gtk.toWindow window
                                         --drawingArea <- Gtk.drawingAreaNew
                                         drawingArea <- getObject Gtk.Widget builder "drawing-area"
                                         playbin <-
                                           (\(Just x) -> x) <$>
                                           Gst.elementFactoryMake "playbin" (Just "MultimediaPlayer")
                                         drawingRef <- newIORef Nothing
                                         _ <-
                                           Gdk.threadsAddTimeout
                                           GLib.PRIORITY_DEFAULT_IDLE
                                           10
                                           (drawingThread window playbin drawingRef >> return True)
                                         --_ <-
                                         --  Gdk.threadsAddIdle
                                         --    GLib.PRIORITY_DEFAULT_IDLE
                                         --    (drawingThread window playbin drawingRef >> return True)
                                         _ <- forkIO $ model subr drawingRef requestChannel
                                         _ <-
                                           Gtk.afterWidgetKeyReleaseEvent
                                           window
                                           (\key -> controller requestChannel key >> return False)
                                         --_ <-
                                         --  Gtk.onWidgetRealize drawingArea $ onDrawingAreaRealize' window playbin
                                         _ <-
                                           Gtk.onWidgetRealize drawingArea $ onDrawingAreaRealize drawingArea playbin
                                         Gtk.widgetShowAll window
  Gtk.main

--onDrawingAreaRealize' :: Gtk.ApplicationWindow -> Gst.Element -> IO ()
--onDrawingAreaRealize' window playbin = do
--  _ <-Gtk.windowGetApplication window >>= putStrLn . (\case Nothing -> "Nothing"; Just _ -> "Just") 
--  Gtk.applicationWindowGetId window >>= GI.GstVideo.videoOverlaySetWindowHandle (GstElement playbin) . traceShowId . fromIntegral

onDrawingAreaRealize :: Gtk.IsWidget a => a -> Gst.Element -> IO ()
onDrawingAreaRealize drawingArea playbin = do
  gdkWindow <- (\(Just x) -> x) <$> Gtk.widgetGetWindow drawingArea
  x11Window <- Gtk.unsafeCastTo GI.GdkX11.X11Window gdkWindow
  xid <- fromIntegral <$> GI.GdkX11.x11WindowGetXid x11Window
  GI.GstVideo.videoOverlaySetWindowHandle (GstElement playbin) xid

  --playbin' <- castTo Gtk.Label playbin >>= \case
  --  Nothing -> fail ""
  --  Just x -> return x
  --GI.GstVideo.videoOverlaySetWindowHandle (playbin') xid

getObject ::
     (GI.GObject.GObject b, Gtk.IsBuilder a)
  => (Data.GI.Base.ManagedPtr b -> b)
  -> a
  -> String
  -> IO b
getObject objectTypeClass builder objectId =
  (\(Just x) -> x) <$> Gtk.builderGetObject builder (pack objectId) >>=
  Gtk.unsafeCastTo objectTypeClass

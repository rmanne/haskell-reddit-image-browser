module Controller
  ( controller
  ) where

import Control.Concurrent.Chan (Chan, writeChan)
import GI.Gdk (EventKey)
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import Types

controller :: Chan Command -> EventKey -> IO ()
controller channel key =
  Gdk.getEventKeyKeyval key >>= Gdk.keyvalName >>=
  (\name -> print name >> return name) >>= \case
    Just "q" -> Gtk.mainQuit
    Just "j" -> writeChan channel Next
    Just "k" -> writeChan channel Prev
    Just "s" -> writeChan channel Toggle
    Just "d" -> writeChan channel Remove
    Just "w" -> writeChan channel Save
    Just "v" -> writeChan channel ToggleDeleted
    Just "r" -> writeChan channel Refresh
    Just "g" -> writeChan channel Front
    Just "G" -> writeChan channel Back
    Just "c" -> writeChan channel Commit
    Just "f" -> writeChan channel FindFailed
    Just "a" -> writeChan channel Status
    Just "l" -> writeChan channel NextImage
    Just "h" -> writeChan channel PrevImage
    Just "0" -> writeChan channel (Multi 0)
    Just "1" -> writeChan channel (Multi 1)
    Just "2" -> writeChan channel (Multi 2)
    Just "3" -> writeChan channel (Multi 3)
    Just "4" -> writeChan channel (Multi 4)
    Just "5" -> writeChan channel (Multi 5)
    Just "6" -> writeChan channel (Multi 6)
    Just "7" -> writeChan channel (Multi 7)
    Just "8" -> writeChan channel (Multi 8)
    Just "9" -> writeChan channel (Multi 9)
    _ -> return ()

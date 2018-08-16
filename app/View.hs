module View
  ( view
  ) where

import Data.GI.Base.Properties (setObjectPropertyString)
import Data.List (isSuffixOf)
import qualified Data.Text as Text
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (utctDay)
import qualified GI.Gst as Gst
import qualified GI.Gtk as Gtk
import qualified Reddit.Types.Listing as R
import qualified Reddit.Types.Post as R
import Types

showFile :: Gst.Element -> FilePath -> IO ()
showFile display file = do
  _ <- Gst.elementSetState display Gst.StateReady
  _ <- Gst.elementGetState display 18446744073709551615 -- wait for state change to complete
  --setObjectPropertyDouble display "volume" 0.0
  _ <-
    setObjectPropertyString display "uri" (Just $ Text.pack $ "file://" ++ file)
  --pipeline <- Gst.unsafeCastTo Gst.Pipeline display
  --bus <- Gst.pipelineGetBus pipeline
  bus <- (\(Just x) -> x) <$> Gst.elementGetBus display
  _ <- Gst.busRemoveWatch bus
  _ <-
    if isSuffixOf ".mp4" file ||
       isSuffixOf ".webm" file || isSuffixOf ".gif" file
      then Gst.busAddWatch
             bus
             0
             (\_ message ->
                Gst.getMessageType message >>= \case
                  x
                    | elem Gst.MessageTypeEos x ->
                      Gst.elementSeekSimple
                        display
                        Gst.FormatTime
                        [Gst.SeekFlagsFlush, Gst.SeekFlagsSegment]
                        0 >>
                      -- Gst.elementSetState display Gst.StateReady >>
                      -- Gst.elementSetState display Gst.StatePlaying >>
                      return True
                  x
                    | elem Gst.MessageTypeSegmentDone x ->
                      Gst.elementSeekSimple
                        display
                        Gst.FormatTime
                        [Gst.SeekFlagsSegment]
                        0 >>
                      return True
                  _ -> return True)
      else Gst.busAddWatch bus 0 (\_ _ -> return True)
  _ <- Gst.elementSetState display Gst.StatePlaying
  _ <- Gst.elementGetState display 18446744073709551615 -- wait for state change to complete
  return ()

view :: Gtk.Window -> Gst.Element -> View -> IO ()
view window display (Deleted (R.PostID p), _) =
  let title = "[Deleted] " ++ show p
   in putStrLn title >> Gtk.setWindowTitle window (Text.pack title) >>
      showFile display "/home/work/hs-reddit-image-browser/loading.jpg"
view window display (Failed (R.PostID p), _) =
  let title = "[Failed] " ++ show p
   in putStrLn title >> Gtk.setWindowTitle window (Text.pack title) >>
      showFile display "/home/work/hs-reddit-image-browser/loading.jpg"
view window display (Submitted post, _) = do
  let title =
        "[Downloading] " ++
        (let (_, month, day) = toGregorian $ utctDay (R.created post)
          in show month ++ "/" ++ show day) ++
        " [score=" ++ show (R.score post) ++ "] " ++ Text.unpack (R.title post)
  putStrLn $ (show $ R.postID post) ++ title
  Gtk.setWindowTitle window $ Text.pack title
  showFile display "/home/work/hs-reddit-image-browser/loading.jpg"
view window display (Downloaded post files, currentIndex) = do
  let currentIndex' = currentIndex `mod` (length files)
  let title =
        (let (_, month, day) = toGregorian $ utctDay (R.created post)
          in show month ++ "/" ++ show day) ++
        " [score=" ++
        show (R.score post) ++
        "] " ++
        "[" ++ show currentIndex' ++ "/" ++ show (length files) ++ "] " ++ Text.unpack (R.title post)
  putStrLn $ (show $ R.postID post) ++ title
  Gtk.setWindowTitle window $ Text.pack title
  showFile display (files !! currentIndex')

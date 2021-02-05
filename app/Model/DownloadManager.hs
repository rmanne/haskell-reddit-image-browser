module Model.DownloadManager
  ( initialize
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, getChanContents, newChan, writeChan)
import Control.Monad (forM_, replicateM_)
import qualified Data.Text as Text
import Data.Text (Text)
import ImageLink (imageLinks)
import Model.Types (Config(aria2Secret, path))
import qualified Network.Aria2 as Aria2
import qualified Reddit as R
import qualified Reddit.Types.Post as R
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath.Posix (takeExtension)
import Types (Command(Download))

initialize :: Config -> Chan Command -> IO (Chan R.Post)
initialize config modelChannel = do
  aria2InputChan <- newChan
  aria2OutputChan <- newChan
  _ <-
    forkIO
      (Aria2.run
         (Aria2.defaultOptions {Aria2.secret = aria2Secret config})
         aria2InputChan
         aria2OutputChan)
  _ <-
    forkIO $
    getChanContents aria2OutputChan >>=
    mapM_
      (\((pid, index, count, fname), result) ->
         case result of
           Left msg ->
             print msg >> writeChan modelChannel (Download pid index count [])
           Right f' ->
             renameFile (Text.unpack f') fname >>
             writeChan modelChannel (Download pid index count [fname]))
  downloadChannel <- newChan
  replicateM_
    6
    (forkIO
       (getChanContents downloadChannel >>=
        mapM_ (downloadPost config modelChannel aria2InputChan)))
  return downloadChannel

downloadPost ::
     Config
  -> Chan Command
  -> Chan ((R.PostID, Int, Int, FilePath), Text)
  -> R.Post
  -> IO ()
downloadPost config channel aria2InputChan p@R.Post { R.content = R.Link txt
                                                    , R.postID = R.PostID pid
                                                    } = do
  let link = Text.unpack txt
  dl <- imageLinks link
  case dl of
    _:_ -> do
      let dname =
            Text.unpack $
            path config <>
            (let R.R subr = R.subreddit p
              in subr)
      createDirectoryIfMissing False dname
      forM_ (zip [1 ..] dl) $ \(i, dl') -> do
        let fname =
              if length dl == 1
                then dname <> "/" <> Text.unpack pid <> takeExtension dl'
                else dname <>
                     "/" <>
                     Text.unpack pid <> "-" <> show i <> takeExtension dl'
        doesFileExist fname >>= \case
          True -> do
            putStrLn $
              "File exists " <> link <> " (" <> dl' <> ") into " <> fname
            writeChan channel (Download (R.postID p) i (length dl) [fname])
          False -> do
            putStrLn $
              "Downloading " <> link <> " (" <> dl' <> ") into " <> fname
            writeChan
              aria2InputChan
              ((R.postID p, i, length dl, fname), Text.pack dl')
    _ -> do
      putStrLn $ "Unable to download " ++ link ++ " " ++ show dl
      writeChan channel (Download (R.postID p) 1 1 [])
downloadPost _ channel _ R.Post {R.postID = i} =
  writeChan channel (Download i 1 1 [])

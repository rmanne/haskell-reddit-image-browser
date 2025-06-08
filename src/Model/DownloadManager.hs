module Model.DownloadManager
  ( initialize,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, getChanContents, newChan, writeChan)
import Control.Monad (forM_, replicateM_)
import Data.Text (Text)
import qualified Data.Text as Text
import ImageLink (DownloadHandler (DirectLink, YoutubeDL), imageLinks)
import Model.Types (Config (aria2Secret, path))
import qualified Network.Aria2 as Aria2
import qualified Reddit as R
import qualified Reddit.Types.Post as R
import System.Directory (createDirectoryIfMissing, doesFileExist, copyFile, removeFile)
import System.FilePath.Posix (takeExtension)
import System.Process (readCreateProcessWithExitCode, shell)
import Types (Command (Download))

initialize :: Config -> Chan Command -> IO (Chan R.Post)
initialize config modelChannel = do
  aria2InputChan <- newChan
  aria2OutputChan <- newChan
  _ <-
    forkIO
      ( Aria2.run
          (Aria2.defaultOptions {Aria2.secret = aria2Secret config})
          aria2InputChan
          aria2OutputChan
      )
  _ <-
    forkIO $
      getChanContents aria2OutputChan
        >>= mapM_
          ( \((pid, index, count, fname), result) ->
              case result of
                Left msg ->
                  print msg >> writeChan modelChannel (Download pid index count [])
                Right f' ->
                  copyFile (Text.unpack f') fname
                    >> removeFile (Text.unpack f')
                    >> writeChan modelChannel (Download pid index count [fname])
          )
  downloadChannel <- newChan
  replicateM_
    6
    ( forkIO
        ( getChanContents downloadChannel
            >>= mapM_ (downloadPost config modelChannel aria2InputChan)
        )
    )
  return downloadChannel

downloadPost ::
  Config ->
  Chan Command ->
  Chan ((R.PostID, Int, Int, FilePath), Text) ->
  R.Post ->
  IO ()
downloadPost
  config
  channel
  aria2InputChan
  p@R.Post
    { R.content = R.Link txt,
      R.postID = R.PostID pid
    } = do
    let link = Text.unpack txt
    dl <- imageLinks link
    case dl of
      _ : _ -> do
        let dname =
              Text.unpack $
                path config
                  <> ( let R.R subr = R.subreddit p
                        in subr
                     )
        createDirectoryIfMissing False dname
        forM_ (zip [1 ..] dl) $ \(i, dl') -> do
          let extension = case snd dl' of
                YoutubeDL -> ".mp4"
                DirectLink -> takeExtensionOnly (fst dl')
          let fname =
                if length dl == 1
                  then dname <> "/" <> Text.unpack pid <> extension
                  else
                    dname
                      <> "/"
                      <> Text.unpack pid
                      <> "-"
                      <> show i
                      <> extension
          doesFileExist fname >>= \case
            True -> do
              putStrLn $
                "File exists " <> link <> " (" <> show dl' <> ") into " <> fname
              writeChan channel (Download (R.postID p) i (length dl) [fname])
            False -> do
              putStrLn $
                "Downloading " <> link <> " (" <> show dl' <> ") into " <> fname
              case snd dl' of
                YoutubeDL ->
                  readCreateProcessWithExitCode
                    (shell $ "yt-dlp -w -o '" <> fname <> "' --newline '" <> fst dl' <> "'")
                    ""
                    >>= ( \(exitCode, stdout, stderr) ->
                            putStrLn (show dl' <> " failed with code: " <> show exitCode)
                              <> putStrLn stdout
                              <> putStrLn ""
                              <> putStrLn stderr
                        )
                    >> doesFileExist fname
                    >>= \case
                      False -> writeChan channel (Download (R.postID p) i (length dl) [])
                      True ->
                        writeChan
                          channel
                          (Download (R.postID p) i (length dl) [fname])
                DirectLink ->
                  writeChan
                    aria2InputChan
                    ((R.postID p, i, length dl, fname), Text.pack (fst dl'))
      _ -> do
        putStrLn $ "Unable to download " ++ link ++ " " ++ show dl ++ " sub=" ++ (let R.R subr = R.subreddit p in show subr)
        writeChan channel (Download (R.postID p) 1 1 [])
downloadPost _ channel _ R.Post {R.postID = i} =
  writeChan channel (Download i 1 1 [])

takeExtensionOnly :: String -> String
takeExtensionOnly = takeWhile (/= '?') . takeExtension

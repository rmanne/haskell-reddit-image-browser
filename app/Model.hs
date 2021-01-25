{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Model
  ( model
  , redditGet
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.Chan
  ( Chan
  , getChanContents
  , newChan
  , writeChan
  , writeList2Chan
  )
import Control.Exception (SomeException(SomeException), catch)
import qualified Control.Lens as Lens
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
import Control.Monad
  ( foldM
  , foldM_
  , forM_
  , forever
  , join
  , mapM_
  , replicateM_
  , when
  )
import Control.Monad.Loops (untilM_)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Aeson (FromJSON, decode)
import Data.IORef (IORef, writeIORef)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Maybe (catMaybes, isJust)
import Data.PointedList
import qualified Data.PointedList
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml as Yaml
import Debug.Trace (trace)
import GHC.Generics (Generic)
import ImageLink
import Lib
import qualified Network.API.Builder.Routes as Routes
import qualified Network.Aria2 as Aria2
import qualified OldPointedList
import qualified Reddit as R
import qualified Reddit.Types.Listing as R
import qualified Reddit.Types.OAuth as R
import qualified Reddit.Types.Options as Options
import qualified Reddit.Types.Post as R
import System.CPUTime (getCPUTime)
import System.Directory
  ( XdgDirectory(XdgConfig)
  , createDirectoryIfMissing
  , doesFileExist
  , getXdgDirectory
  , removeFile
  , renameFile
  )
import System.FilePath.Posix (takeExtension)
import Text.Read (readEither)
import Types

--import Control.Monad.Extra(whenM)
whenM :: Monad m => m Bool -> m () -> m ()
whenM condition action =
  condition >>= \case
    True -> action
    False -> return ()

postId :: Post -> R.PostID
postId (Downloaded R.Post {R.postID = p} _) = p
postId (Submitted R.Post {R.postID = p}) = p
postId (Failed p) = p
postId (Deleted p) = p

data SpecialArg =
  SpecialArg
    { route :: [Text]
    , refreshToken :: Text
    }
  deriving (Generic)

data Config =
  Config
    { userAgent :: Text
    , clientId :: Text
    , secret :: Text
    , path :: Text
    , specialArgs :: Map Text SpecialArg
    , aria2Secret :: Maybe Text
    }
  deriving (Generic)

instance FromJSON SpecialArg

instance FromJSON Config

data Model =
  Model
    { _skipDeleted :: Bool
    , _posts :: PointedList Post
    , _subredditName :: R.SubredditName
    , _downloaded :: Map R.PostID [FilePath]
    , _downloadedMulti :: Map (R.PostID, Int) FilePath
    , _downloadChannel :: Chan R.Post
    , _slideshowThread :: Maybe ThreadId
    , _multi :: Int
    , _currentIndex :: Int
    , _config :: Config
    }

$(makeLenses ''Model)

save :: ModelM ()
save = do
  startTime <- lift getCPUTime
  posts <- use posts
  R.R subreddit <- use subredditName
  config <- use config
  lift $
    writeFile
      (Text.unpack (path config) <> Text.unpack subreddit <> ".conf")
      (show posts)
  endTime <- lift getCPUTime
  lift $
    putStrLn $
    "Saved reddit in " ++ show ((endTime - startTime) `div` 1000000000) ++ "ms"

load :: Config -> String -> Chan R.Post -> IO (PointedList Post)
load config subr dlchannel =
  doesFileExist fileName >>= \case
    False ->
      Data.PointedList.fromList <$>
      redditGet config (R.R $ Text.pack subr) Nothing dlchannel
    True ->
      readEither <$> (readFile fileName `catch` \(SomeException _) -> return "") >>= \case
        Left message1 -> do
          putStrLn "Loading Failed, trying old loading..."
          readEither <$>
            (readFile fileName `catch` \(SomeException _) -> return "") >>= \case
            Left message2 ->
              putStrLn
                ("Loading Failed, file: " <>
                 (Text.unpack (path config) <> subr <> ".conf")) >>
              putStrLn message1 >>
              putStrLn message2 >>
              fail "Loading failed"
            Right oldList ->
              let list = OldPointedList.conv oldList
               in mapM_
                    (\case
                       Submitted p -> writeChan dlchannel p
                       _ -> return ())
                    list >>
                  return list
        Right list ->
          mapM_
            (\case
               Submitted p -> writeChan dlchannel p
               _ -> return ())
            list >>
          return list
  where
    fileName = Text.unpack (path config) <> subr <> ".conf"

redditGetT :: Maybe (R.PaginationOption R.PostID) -> ModelM [Post]
redditGetT options =
  join $
  (\config subredditName downloadChannel ->
     lift $ redditGet config subredditName options downloadChannel) <$>
  use config <*>
  use subredditName <*>
  use downloadChannel

redditGet ::
     Config
  -> R.SubredditName
  -> Maybe (R.PaginationOption R.PostID)
  -> Chan R.Post
  -> IO [Post]
redditGet config subreddit@(R.R arg) option dlchannel =
  R.runRedditWith redditOptions redditAction >>= \case
    Right (R.Listing before' after' contents) -> do
      print option
      print $ map R.postID contents
      print before'
      print after'
      _ <-
        forkIO $
        writeList2Chan dlchannel $
        filter
          (\p ->
             case R.content p of
               R.Link _ -> True
               _ -> False)
          contents
      return $
        map
          (\p ->
             case R.content p of
               R.Link _ -> Submitted p
               _ -> Deleted (R.postID p))
          contents
    Left msg -> do
      putStrLn $ "Unable to get newer: " ++ show msg
      return []
  where
    (redditOptions, redditAction) =
      redditOptionsFor config arg & _2 %~ ($ option)

redditOptionsFor ::
     Config
  -> Text
  -> ( R.RedditOptions
     , Maybe (R.PaginationOption R.PostID) -> R.RedditT IO R.PostListing)
redditOptionsFor config subreddit =
  case Map.lookup subreddit (specialArgs config) of
    Nothing ->
      ( R.defaultRedditOptions
          {R.customUserAgent = Just (Text.encodeUtf8 $ userAgent config)}
      , \option ->
          R.getPosts'
            R.Options {R.pagination = option, R.limit = Just 100}
            R.New
            (Just $ R.R subreddit))
    Just SpecialArg {refreshToken, route} ->
      ( R.defaultRedditOptions
          { R.customUserAgent = Just (Text.encodeUtf8 $ userAgent config)
          , R.loginMethod =
              R.OAuth
                R.Script
                  { R.clientId = clientId config
                  , R.secret = secret config
                  , R.redirectUrl = ""
                  }
                (R.RefreshToken refreshToken)
          }
      , \option ->
          let opts = R.Options {R.pagination = option, R.limit = Just 100}
           in R.runRoute
                (Routes.Route
                   route
                   [ "limit" Routes.=. Options.limit opts
                   , "before" Routes.=. Options.before opts
                   , "after" Routes.=. Options.after opts
                   ]
                   "GET"))

refresh :: ModelM ()
refresh =
  (^. current) <$> use posts >>= \case
    Downloaded post _ -> downloadLink post
    Submitted _ -> return () -- do nothing, is already submitted
    Deleted postId -> downloadPost postId
    Failed postId -> downloadPost postId
  where
    downloadLink :: R.Post -> ModelM ()
    downloadLink post =
      use downloadChannel >>= \channel ->
        lift (writeChan channel post) >> posts %= (current .~ Submitted post)
    downloadPost :: R.PostID -> ModelM ()
    downloadPost postId =
      redditOptionsFor <$> use config <*>
      ((\(R.R arg) -> arg) <$> use subredditName) >>= \(redditOptions, _) ->
        R.runRedditWith redditOptions (R.getPostInfo postId) >>= \case
          Left msg ->
            lift (putStrLn ("Failed to download " <> show postId)) >>
            lift (putStrLn ("Message: " <> show msg))
          Right post ->
            case R.content post of
              R.Link _ -> downloadLink post
              _ -> return ()

type ModelM = State.StateT Model IO

isDeleted :: Post -> Bool
isDeleted (Deleted _) = True
isDeleted _ = False

next :: ModelM ()
next =
  goNext <$> use posts >>= \case
    Just posts' ->
      posts .= posts' >>
      whenM
        (use skipDeleted)
        (if isDeleted (posts' ^. current)
           then next
           else currentIndex .= 0)
    Nothing ->
      Just . R.Before . postId . (^. current) <$> use posts >>= redditGetT >>= \case
        [] -> return ()
        elements -> posts %= addToBack elements >> next

nextUntilFailed :: ModelM ()
nextUntilFailed =
  all (not . isFailed) . (^. back) <$> use posts >>= \case
    True -> return ()
    False -> next `untilM_` (isFailed . (^. current) <$> use posts)
  where
    isFailed (Failed _) = True
    isFailed _ = False

prev :: ModelM ()
prev =
  goPrev <$> use posts >>= \case
    Just posts' ->
      posts .= posts' >>
      whenM
        (use skipDeleted)
        (if isDeleted (posts' ^. current)
           then prev
           else currentIndex .= 0)
    Nothing ->
      (^. current) <$> use posts >>= \currentPost ->
        redditGetT (Just $ R.Before $ postId currentPost) >>= \case
          [] ->
            Just . R.Before . postId . head . (^. back) <$> use posts >>=
            redditGetT >>= {- Maybe currentPost is deleted, so try the next one as well -}
             \case
              [] -> return () -- both posts might be deleted, just give up
              elements
                | all
                   (\element -> postId element /= postId currentPost)
                   elements -> posts %= addToFront elements >> prev
              _ -> return () -- current isn't deleted, just return even if we can do better
          elements -> posts %= addToFront elements >> prev

slideshowWorker :: Chan Command -> IO ()
slideshowWorker channel =
  threadDelay 3000000 >> writeChan channel Next >> slideshowWorker channel

display :: IORef (Maybe View) -> ModelM ()
display ref =
  (,) <$> ((^. current) <$> use posts) <*> use currentIndex >>=
  lift . writeIORef ref . Just

processAndDisplay :: IORef (Maybe View) -> ModelM ()
processAndDisplay ref = commitState >> display ref
  where
    commitState :: ModelM ()
    commitState =
      (^. current) <$> use posts >>= \case
        (Submitted post) ->
          Map.updateLookupWithKey (\_ _ -> Nothing) (R.postID post) <$>
          use downloaded >>= \case
            (Nothing, _) -> return ()
            (Just [], newMap) ->
              posts %= (current .~ Failed (R.postID post)) >>
              downloaded .= newMap
            (Just files, newMap) ->
              posts %= (current .~ Downloaded post files) >>
              downloaded .= newMap
        _ -> return ()

saveSender :: Chan Command -> IO ()
saveSender channel =
  threadDelay 10000000 >> writeChan channel Save >> saveSender channel

model :: String -> IORef (Maybe View) -> Chan Command -> IO ()
model subr ref channel = do
  config <-
    getXdgDirectory XdgConfig "hs-reddit-image-browser/config.yaml" >>=
    Yaml.decodeFileEither >>= \case
      Left msg -> fail $ show msg
      Right v -> return v
  _ <- forkIO $ saveSender channel
  dlchannel <- newChan
  initialPosts <- load config subr dlchannel
  let initialState =
        Model
          { _posts = initialPosts
          , _subredditName = R.R (Text.pack subr)
          , _downloadChannel = dlchannel
          , _skipDeleted = True
          , _downloaded = Map.empty
          , _downloadedMulti = Map.empty
          , _slideshowThread = Nothing
          , _multi = 1
          , _currentIndex = 0
          , _config = config
          }
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
    forever $
    getChanContents aria2OutputChan >>=
    mapM_
      (\((pid, index, length, fname), result) ->
         case result of
           Left msg ->
             print msg >> writeChan channel (Download pid index length [])
           Right f' ->
             renameFile (Text.unpack f') fname >>
             writeChan channel (Download pid index length [fname]))
  _ <-
    replicateM_
      6
      (forkIO (downloadWorker config channel dlchannel aria2InputChan))
  State.evalStateT
    (display ref >> lift (getChanContents channel) >>=
     mapM_ (model' ref channel))
    initialState
  where
    fillPost :: Map R.PostID [FilePath] -> Post -> Post
    fillPost map (Submitted p) =
      case Map.lookup (R.postID p) map of
        Just f
          | not (null f) -> Downloaded p f
        Just [] -> Failed (R.postID p)
        Nothing -> Submitted p
    fillPost map p = p

model' :: IORef (Maybe View) -> Chan Command -> Command -> ModelM ()
model' ref channel =
  \case
    Next -> use multi >>= flip replicateM_ next >> multi .= 1
    Prev -> use multi >>= flip replicateM_ prev >> multi .= 1
    Toggle ->
      use slideshowThread >>= \case
        Nothing ->
          lift (forkIO (slideshowWorker channel)) >>= \threadId ->
            slideshowThread .= Just threadId >> multi .= 1
        Just threadId ->
          lift (killThread threadId) >> slideshowThread .= Nothing >> multi .= 1
    Remove ->
      (^. current) <$> use posts >>= \case
        Downloaded post files ->
          mapM_
            (\file ->
               lift (removeFile file `catch` (\SomeException {} -> return ())))
            files >>
          posts %= (current .~ Deleted (R.postID post)) >>
          multi .= 1 >>
          next >>
          processAndDisplay ref
        Failed postId ->
          posts %= (current .~ Deleted postId) >> multi .= 1 >> next >>
          processAndDisplay ref
        _ -> multi .= 1
    Save -> save
    ToggleDeleted -> skipDeleted %= not >> multi .= 1
    Refresh -> refresh >> multi .= 1 >> currentIndex .= 0
    Front ->
      posts %= goToFront >> multi .= 1 >> currentIndex .= 0 >>
      processAndDisplay ref
    Back ->
      posts %= goToBack >> multi .= 1 >> currentIndex .= 0 >>
      processAndDisplay ref
    Commit ->
      use downloaded >>= \dlmap ->
        posts %= fmap (fillPost dlmap) >> multi .= 1 >> downloaded .= Map.empty
    FindFailed -> nextUntilFailed >> multi .= 1 >> processAndDisplay ref
    Status ->
      multi .= 1 >> use downloaded >>=
      lift . putStrLn . ("Downloaded: " <>) . show
    NextImage ->
      use multi >>= (currentIndex +=) >> multi .= 1 >> processAndDisplay ref
    PrevImage ->
      use multi >>= (currentIndex -=) >> multi .= 1 >> processAndDisplay ref
    Multi num -> multi .= num
    Download postId 1 1 file -- single-file post
     -> downloaded %= Map.insert postId file >> processAndDisplay ref
    Download postId index length [file] ->
      downloadedMulti %= Map.insert (postId, index) file >>
      (\multiMap ->
         map (\index -> Map.lookup (postId, index) multiMap) [1 .. length]) <$>
      use downloadedMulti >>= \results ->
        when
          (all isJust results)
          (downloaded %= Map.insert postId (catMaybes results) >>
           mapM_
             (\index -> downloadedMulti %= Map.delete (postId, index))
             [1 .. length]) >>
        processAndDisplay ref
  where
    fillPost :: Map R.PostID [FilePath] -> Post -> Post
    fillPost map (Submitted p) =
      case Map.lookup (R.postID p) map of
        Just f
          | not (null f) -> Downloaded p f
        Just [] -> Failed (R.postID p)
        Nothing -> Submitted p
    fillPost map p = p

downloadWorker ::
     Config
  -> Chan Command
  -> Chan R.Post
  -> Chan ((R.PostID, Int, Int, FilePath), Text)
  -> IO ()
downloadWorker config channel dlchannel aria2InputChan =
  getChanContents dlchannel >>=
  mapM_ (downloadPost config channel aria2InputChan)

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
downloadPost config channel aria2InputChan R.Post {R.postID = i} =
  writeChan channel (Download i 1 1 [])

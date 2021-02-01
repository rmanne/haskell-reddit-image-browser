{-# LANGUAGE TemplateHaskell #-}

module Model
  ( model
  ) where

import Control.Applicative (liftA2)
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.Chan
  ( Chan
  , getChanContents
  , writeChan
  , writeList2Chan
  )
import Control.Exception (SomeException(SomeException), catch)
import Control.Lens ((%=), (+=), (-=), (.=), (.~), (^.), makeLenses, use, uses)
import Control.Monad (forever, join, mapM_, replicateM_, when)
import Control.Monad.Extra (whenM)
import Control.Monad.Loops (untilM_)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Function.Extra ((...))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (catMaybes, isJust)
import Data.PointedList
  ( PointedList
  , addToBack
  , addToFront
  , back
  , current
  , goNext
  , goPrev
  , goToBack
  , goToFront
  )
import qualified Data.PointedList
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (utctDay)
import qualified Data.Yaml as Yaml
import qualified Model.DownloadManager as DownloadManager
import qualified Model.Reddit as Reddit
import Model.Types (Config(path))
import qualified OldPointedList
import qualified Reddit as R
import qualified Reddit.Types.Post as R
import System.CPUTime (getCPUTime)
import System.Directory
  ( XdgDirectory(XdgConfig)
  , doesFileExist
  , getXdgDirectory
  , removeFile
  )
import Text.Read (readEither)
import Types
  ( Command(Back, Commit, Download, FindFailed, Front, Multi, Next,
        NextImage, Prev, PrevImage, Refresh, Remove, Save, Status, Toggle,
        ToggleDeleted)
  , Post(Deleted, Downloaded, Failed, Submitted)
  )
import qualified View.Types as View

postIdOf :: Post -> R.PostID
postIdOf (Downloaded R.Post {R.postID = p} _) = p
postIdOf (Submitted R.Post {R.postID = p}) = p
postIdOf (Failed p) = p
postIdOf (Deleted p) = p

data Model =
  Model
    { _skipDeleted :: Bool
    , _posts :: PointedList Post
    , _subredditName :: Text
    , _downloaded :: Map R.PostID [FilePath]
    , _downloadedMulti :: Map (R.PostID, Int) FilePath
    , _downloadChannel :: Chan R.Post
    , _slideshowThread :: Maybe ThreadId
    , _multi :: Int
    , _currentIndex :: Int
    , _config :: Config
    , _viewChannel :: Chan View.Action
    }

$(makeLenses ''Model)

save :: ModelM ()
save = do
  startTime <- lift getCPUTime
  join $
    lift ... writeFile <$> (Text.unpack <$> fileName) <*> (show <$> use posts)
  endTime <- lift getCPUTime
  lift $
    putStrLn $
    "Saved reddit in " ++ show ((endTime - startTime) `div` 1000000000) ++ "ms"
  where
    fileName :: ModelM Text
    fileName = uses config path <-> use subredditName <-> pure ".conf"
    (<->) :: (Monad m, Semigroup a) => m a -> m a -> m a
    (<->) = liftA2 (<>)

load :: Config -> Text -> Chan R.Post -> IO (PointedList Post)
load cfg subr dlchannel =
  doesFileExist fileName >>= \case
    False ->
      Reddit.list cfg subr Nothing >>= \p ->
        writeList2Chan dlchannel p >>
        return (Data.PointedList.fromList (Submitted <$> p))
    True ->
      readEither <$> (readFile fileName `catch` \(SomeException _) -> return "") >>= \case
        Left message1 -> do
          putStrLn "Loading Failed, trying old loading..."
          readEither <$>
            (readFile fileName `catch` \(SomeException _) -> return "") >>= \case
            Left message2 ->
              putStrLn ("Loading Failed, file: " <> fileName) >>
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
    fileName = Text.unpack (path cfg) <> Text.unpack subr <> ".conf"

redditGetT :: Maybe (R.PaginationOption R.PostID) -> ModelM [Post]
redditGetT options =
  join
    (lift ... Reddit.list <$> use config <*> use subredditName <*> pure options) >>= \p ->
    download p >> return (Submitted <$> p)

download :: [R.Post] -> ModelM ()
download p = join (lift ... writeList2Chan <$> use downloadChannel <*> pure p)

refresh :: ModelM ()
refresh =
  (^. current) <$> use posts >>= \case
    Downloaded post _ -> download [post] >> posts %= (current .~ Submitted post)
    Submitted _ -> return () -- do nothing, is already submitted
    Deleted postId -> downloadPost postId
    Failed postId -> downloadPost postId
  where
    downloadPost :: R.PostID -> ModelM ()
    downloadPost postId =
      join
        (lift ... Reddit.post <$> use config <*> use subredditName <*>
         pure postId) >>= \case
        Nothing -> return ()
        Just post -> download [post] >> posts %= (current .~ Submitted post)

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
      Just . R.Before . postIdOf . (^. current) <$> use posts >>= redditGetT >>= \case
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
        redditGetT (Just $ R.Before $ postIdOf currentPost) >>= \case
          [] ->
            Just . R.Before . postIdOf . head . (^. back) <$> use posts >>=
            redditGetT >>= {- Maybe currentPost is deleted, so try the next one as well -}
             \case
              [] -> return () -- both posts might be deleted, just give up
              elements
                | all
                   (\element -> postIdOf element /= postIdOf currentPost)
                   elements -> posts %= addToFront elements >> prev
              _ -> return () -- current isn't deleted, just return even if we can do better
          elements -> posts %= addToFront elements >> prev

slideshowWorker :: Chan Command -> IO ()
slideshowWorker channel =
  threadDelay 3000000 >> writeChan channel Next >> slideshowWorker channel

viewToTitleFile :: (Post, Int) -> (String, Maybe FilePath)
viewToTitleFile (Deleted (R.PostID post), _) =
  ("[Deleted] " <> show post, Nothing)
viewToTitleFile (Failed (R.PostID post), _) =
  ("[Failed] " <> show post, Nothing)
viewToTitleFile (Submitted post, _) =
  ( "[Downloading] " <>
    (let (_, month, day) = toGregorian $ utctDay (R.created post)
      in show month <> "/" <> show day) <>
    " [score=" <> show (R.score post) <> "] " <> Text.unpack (R.title post)
  , Nothing)
viewToTitleFile (Downloaded post files, index) =
  let index' = index `mod` length files
   in ( (let (_, month, day) = toGregorian $ utctDay (R.created post)
          in show month <> "/" <> show day) <>
        " [score=" <>
        show (R.score post) <>
        "] [" <>
        show index' <>
        "/" <>
        show (length files) <>
        "] [" <>
        (let R.PostID postId = R.postID post
          in Text.unpack postId) <>
        "] " <> Text.unpack (R.title post)
      , Just (files !! index'))

display :: ModelM ()
display =
  use viewChannel >>= \channel ->
    (,) <$> ((^. current) <$> use posts) <*> use currentIndex >>=
    lift .
    (\(title, file) -> writeChan channel (View.Update title file)) .
    viewToTitleFile

processAndDisplay :: ModelM ()
processAndDisplay = maybeProcessAndDisplay True

maybeProcessAndDisplay :: Bool -> ModelM ()
maybeProcessAndDisplay =
  \case
    True -> commitState >> display
    False -> whenM commitState display
  where
    commitState :: ModelM Bool
    commitState =
      (^. current) <$> use posts >>= \case
        (Submitted post) ->
          Map.updateLookupWithKey (\_ _ -> Nothing) (R.postID post) <$>
          use downloaded >>= \case
            (Nothing, _) -> return False
            (Just [], newMap) ->
              posts %= (current .~ Failed (R.postID post)) >>
              downloaded .= newMap >>
              return False
            (Just files, newMap) ->
              posts %= (current .~ Downloaded post files) >>
              downloaded .= newMap >>
              return False
        _ -> return False

model :: String -> Chan Command -> Chan View.Action -> IO ()
model subr channel viewChan = do
  cfg <-
    getXdgDirectory XdgConfig "hs-reddit-image-browser/config.yaml" >>=
    Yaml.decodeFileEither >>= \case
      Left msg -> fail $ show msg
      Right v -> return v
  _ <- forkIO $ forever $ threadDelay 10000000 >> writeChan channel Save
  dlchannel <- DownloadManager.initialize cfg channel
  initialPosts <- load cfg (Text.pack subr) dlchannel
  let initialState =
        Model
          { _posts = initialPosts
          , _subredditName = Text.pack subr
          , _downloadChannel = dlchannel
          , _skipDeleted = True
          , _downloaded = Map.empty
          , _downloadedMulti = Map.empty
          , _slideshowThread = Nothing
          , _multi = 1
          , _currentIndex = 0
          , _config = cfg
          , _viewChannel = viewChan
          }
  State.evalStateT
    (display >> lift (getChanContents channel) >>= mapM_ (model' channel))
    initialState

model' :: Chan Command -> Command -> ModelM ()
model' channel =
  \case
    Next ->
      use multi >>= flip replicateM_ next >> processAndDisplay >> multi .= 1
    Prev ->
      use multi >>= flip replicateM_ prev >> processAndDisplay >> multi .= 1
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
          processAndDisplay
        Failed postId ->
          posts %= (current .~ Deleted postId) >> multi .= 1 >> next >>
          processAndDisplay
        _ -> multi .= 1
    Save -> save
    ToggleDeleted -> skipDeleted %= not >> multi .= 1
    Refresh -> refresh >> multi .= 1 >> currentIndex .= 0
    Front ->
      posts %= goToFront >> multi .= 1 >> currentIndex .= 0 >> processAndDisplay
    Back ->
      posts %= goToBack >> multi .= 1 >> currentIndex .= 0 >> processAndDisplay
    Commit ->
      use downloaded >>= \dlmap ->
        posts %= fmap (fillPost dlmap) >> multi .= 1 >> downloaded .= Map.empty
    FindFailed -> nextUntilFailed >> multi .= 1 >> processAndDisplay
    Status ->
      multi .= 1 >> use downloaded >>=
      lift . putStrLn . ("Downloaded: " <>) . show
    NextImage -> use multi >>= (currentIndex +=) >> multi .= 1 >> display
    PrevImage -> use multi >>= (currentIndex -=) >> multi .= 1 >> display
    Multi num -> multi .= num
    Download postId 1 1 file -- single-file post
     -> downloaded %= Map.insert postId file >> maybeProcessAndDisplay False
    Download postId index count [file] ->
      downloadedMulti %= Map.insert (postId, index) file >>
      checkMultiMap postId count >>
      maybeProcessAndDisplay False
    action@Download {} -> fail $ "Unexpected Download action: " <> show action
  where
    checkMultiMap :: R.PostID -> Int -> ModelM ()
    checkMultiMap postId count =
      (\multiMap ->
         map (\index -> Map.lookup (postId, index) multiMap) [1 .. count]) <$>
      use downloadedMulti >>= \results ->
        when
          (all isJust results)
          (downloaded %= Map.insert postId (catMaybes results) >>
           mapM_
             (\index -> downloadedMulti %= Map.delete (postId, index))
             [1 .. count])
    fillPost :: Map R.PostID [FilePath] -> Post -> Post
    fillPost m (Submitted p) =
      case Map.lookup (R.postID p) m of
        Just f@(_:_) -> Downloaded p f
        Just [] -> Failed (R.postID p)
        Nothing -> Submitted p
    fillPost _ p = p

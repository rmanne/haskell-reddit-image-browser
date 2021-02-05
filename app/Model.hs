{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Model
  ( model
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.Chan (Chan, readChan, writeChan, writeList2Chan)
import Control.Exception (Exception, SomeException(SomeException), catch)
import Control.Lens ((%=), (+=), (-=), (.=), (.~), (^.), makeLenses, use, uses)
import Control.Lens.Setter ((<~))
import Control.Monad (forever, join, replicateM_, when)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Extra (whenM, whileM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Loops (untilM_)
import Control.Monad.State.Strict (MonadState, evalStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Either (isRight)
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
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (utctDay)
import qualified Data.Yaml as Yaml
import Model.Config (load, save)
import qualified Model.DownloadManager as DownloadManager
import qualified Model.Reddit as Reddit
import Model.Types (Config)
import qualified Reddit as R
import qualified Reddit.Types.Post as R
import System.Directory (XdgDirectory(XdgConfig), getXdgDirectory, removeFile)
import Types
  ( Command(Back, Commit, Download, FindFailed, Front, Multi, Next,
        NextImage, Prev, PrevImage, Quit, Refresh, Remove, Save, Status,
        Toggle, ToggleDeleted)
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
    , _multi :: Maybe Int
    , _currentIndex :: Int
    , _config :: Config
    , _viewChannel :: Chan View.Action
    , _modelChannel :: Chan Command
    , _previousView :: (R.PostID, Int)
    }

$(makeLenses ''Model)

redditGetT ::
     (MonadState Model m, MonadIO m)
  => Maybe (R.PaginationOption R.PostID)
  -> m [Post]
redditGetT options =
  join
    (liftIO ... Reddit.list <$> use config <*> use subredditName <*>
     pure options) >>= \p -> download p >> return (Submitted <$> p)

download :: (MonadState Model m, MonadIO m) => [R.Post] -> m ()
download p = join (liftIO ... writeList2Chan <$> use downloadChannel <*> pure p)

refresh :: (MonadState Model m, MonadIO m) => m ()
refresh =
  uses posts (^. current) >>= \case
    Downloaded post _ -> download [post] >> posts %= (current .~ Submitted post)
    Submitted _ -> return () -- do nothing, is already submitted
    Deleted postId -> downloadPost postId
    Failed postId -> downloadPost postId
  where
    downloadPost :: (MonadState Model m, MonadIO m) => R.PostID -> m ()
    downloadPost postId =
      join
        (liftIO ... Reddit.post <$> use config <*> use subredditName <*>
         pure postId) >>= \case
        Nothing -> return ()
        Just post -> download [post] >> posts %= (current .~ Submitted post)

next :: (MonadState Model m, MonadIO m) => m ()
next = goLoading goNext loadNext
  where
    loadNext :: (MonadState Model m, MonadIO m) => m Bool
    loadNext =
      uses posts (Just . R.After . postIdOf . (^. current)) >>= redditGetT >>= \case
        [] -> return False
        elements -> posts %= addToBack elements >> return True

nextUntilFailed :: MonadState Model m => m ()
nextUntilFailed =
  whenM
    (uses posts (any isFailed . (^. back)))
    (goLoading goNext noLoad `untilM_` uses posts (isFailed . (^. current)))
  where
    isFailed (Failed _) = True
    isFailed _ = False

goLoading ::
     MonadState Model m
  => (PointedList Post -> Maybe (PointedList Post))
  -> m Bool
  -> m ()
goLoading go loader =
  uses posts go >>= \case
    Just posts' ->
      posts .= posts' >>
      whenM
        (use skipDeleted)
        (case posts' ^. current of
           Deleted _ -> goLoading go loader
           _ -> currentIndex .= 0)
    Nothing -> whenM loader (goLoading go loader)

noLoad :: MonadState Model m => m Bool
noLoad = return False

prev :: (MonadState Model m, MonadIO m) => m ()
prev = goLoading goPrev loadPrev
  where
    loadPrev :: (MonadState Model m, MonadIO m) => m Bool
    loadPrev =
      uses posts (^. current) >>= \currentPost ->
        redditGetT (Just (R.Before (postIdOf currentPost))) >>= \case
          []
            -- Maybe currentPost is deleted, so try the next one as well
           ->
            uses posts (Just . R.Before . postIdOf . head . (^. back)) >>=
            redditGetT >>= \case
              [] -> return False
              elements
                | all
                   (\element -> postIdOf element /= postIdOf currentPost)
                   elements -> posts %= addToFront elements >> return True
              _ -> return False -- current isn't deleted, just return even if we can do better
          elements -> posts %= addToFront elements >> return True

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

display :: (MonadState Model m, MonadIO m) => m ()
display =
  use viewChannel >>= \channel ->
    (,) <$> uses posts (^. current) <*> use currentIndex >>=
    liftIO .
    (\(title, file) -> writeChan channel (View.Update title file)) .
    viewToTitleFile

commitCurrent :: MonadState Model m => m Bool
commitCurrent =
  uses posts (^. current) >>= \case
    (Submitted post) ->
      uses
        downloaded
        (Map.updateLookupWithKey (\_ _ -> Nothing) (R.postID post)) >>= \case
        (Nothing, _) -> return False
        (Just [], newMap) ->
          posts %= (current .~ Failed (R.postID post)) >> downloaded .= newMap >>
          return False
        (Just files, newMap) ->
          posts %= (current .~ Downloaded post files) >> downloaded .= newMap >>
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
          , _multi = Nothing
          , _currentIndex = 0
          , _config = cfg
          , _viewChannel = viewChan
          , _modelChannel = channel
          , _previousView = (postIdOf (initialPosts ^. current), 0)
          }
  evalStateT
    (display >>
     whileM
       (liftIO (readChan channel) >>= \command ->
          liftIO (print command) >>
          isRight <$> runExceptT (actionHandler command)))
    initialState

basicUserActionHandler ::
     (MonadError ActionHandlerException m, MonadState Model m, MonadIO m)
  => Command
  -> m ()
basicUserActionHandler Next = next
basicUserActionHandler Prev = prev
basicUserActionHandler Toggle =
  use slideshowThread >>= \case
    Nothing ->
      use modelChannel >>= liftIO . forkIO . slideshowWorker >>= \threadId ->
        slideshowThread .= Just threadId
    Just threadId -> liftIO (killThread threadId) >> slideshowThread .= Nothing
basicUserActionHandler Remove =
  uses posts (^. current) >>= \case
    Downloaded post files ->
      mapM_
        (\file ->
           liftIO (removeFile file `catch` (\SomeException {} -> return ())))
        files >>
      posts %= (current .~ Deleted (R.postID post)) >>
      goLoading goNext noLoad
    Failed postId ->
      posts %= (current .~ Deleted postId) >> goLoading goNext noLoad
    _ -> return ()
basicUserActionHandler Save =
  join $ save <$> use config <*> use subredditName <*> use posts
basicUserActionHandler ToggleDeleted = skipDeleted %= not
basicUserActionHandler Refresh = refresh
basicUserActionHandler Front = posts %= goToFront
basicUserActionHandler Back = posts %= goToBack
basicUserActionHandler FindFailed = nextUntilFailed
basicUserActionHandler Commit =
  use downloaded >>= \dlmap ->
    posts %= fmap (fillPost dlmap) >> downloaded .= Map.empty
  where
    fillPost :: Map R.PostID [FilePath] -> Post -> Post
    fillPost m (Submitted p) =
      case Map.lookup (R.postID p) m of
        Just f@(_:_) -> Downloaded p f
        Just [] -> Failed (R.postID p)
        Nothing -> Submitted p
    fillPost _ p = p
basicUserActionHandler Status =
  use downloaded >>= liftIO . putStrLn . ("Downloaded: " <>) . show
basicUserActionHandler NextImage = currentIndex += 1
basicUserActionHandler PrevImage = currentIndex -= 1
basicUserActionHandler (Multi n) = multi .= Just n
basicUserActionHandler _ = throwError DidNotHandleException

multiUserActionHandler ::
     (MonadError ActionHandlerException m, MonadState Model m, MonadIO m)
  => Int
  -> Command
  -> m ()
multiUserActionHandler n Next = replicateM_ n next
multiUserActionHandler n Prev = replicateM_ n prev
multiUserActionHandler n (Multi digit) = multi .= Just (n * 10 + digit)
multiUserActionHandler _ _ = throwError DidNotHandleException

data ActionHandlerException =
  DidNotHandleException
  deriving (Show)

data QuittingException =
  QuittingException
  deriving (Show)

instance Exception QuittingException

actionHandler ::
     (MonadError () m, MonadState Model m, MonadIO m) => Command -> m ()
actionHandler Quit = throwError ()
actionHandler (Download postId 1 1 file) -- single-file post
 = downloaded %= Map.insert postId file >> commitAndDisplay
actionHandler (Download postId index count [file]) =
  downloadedMulti %= Map.insert (postId, index) file >>
  checkMultiMap postId count >>
  commitAndDisplay
actionHandler action@Download {} =
  liftIO (fail $ "Unexpected Download action: " <> show action)
actionHandler action =
  use multi >>= \case
    Nothing ->
      runExceptT (basicUserActionHandler action) >>= \case
        Left DidNotHandleException ->
          liftIO (putStrLn ("Unexpected action: " <> show action))
        Right () -> checkAndDisplay
    Just n ->
      multi .= Nothing >> runExceptT (multiUserActionHandler n action) >>= \case
        Left DidNotHandleException ->
          liftIO
            (putStrLn
               ("Unexpected combination of actions: " <>
                show n <> " " <> show action))
        Right () -> checkAndDisplay

checkAndDisplay :: (MonadState Model m, MonadIO m) => m ()
checkAndDisplay =
  uses posts (postIdOf . (^. current)) >>= \currentPostId ->
    use previousView >>= \(previousPostId, previousIndex) ->
      (if currentPostId == previousPostId
         then whenM (uses currentIndex (/= previousIndex)) display
         else commitCurrent >> display) >>
      previousView <~ (currentPostId, ) <$> use currentIndex

commitAndDisplay :: (MonadState Model m, MonadIO m) => m ()
commitAndDisplay =
  whenM commitCurrent display >>
  previousView <~ (,) <$> uses posts (postIdOf . (^. current)) <*>
  use currentIndex

checkMultiMap :: MonadState Model m => R.PostID -> Int -> m ()
checkMultiMap postId count =
  uses
    downloadedMulti
    (\multiMap ->
       map (\index -> Map.lookup (postId, index) multiMap) [1 .. count]) >>= \results ->
    when
      (all isJust results)
      (downloaded %= Map.insert postId (catMaybes results) >>
       mapM_
         (\index -> downloadedMulti %= Map.delete (postId, index))
         [1 .. count])

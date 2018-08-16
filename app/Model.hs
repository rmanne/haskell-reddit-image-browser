{-# LANGUAGE DeriveGeneric #-}

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
import Control.Monad (foldM, foldM_, forM_, forever, mapM_)
import Data.Aeson (FromJSON, decode)
import Data.IORef (IORef, writeIORef)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
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

postId :: Post -> R.PostID
postId (Downloaded R.Post {R.postID = p} _) = p
postId (Submitted R.Post {R.postID = p}) = p
postId (Failed p) = p
postId (Deleted p) = p

data PointedList a =
  PointedList
    { front :: [a]
    , current :: a
    , back :: [a]
    }
  deriving (Show, Read)

data SpecialArg =
  SpecialArg
    { route :: [String]
    , refreshToken :: String
    }
  deriving (Generic)

data Config =
  Config
    { userAgent :: String
    , clientId :: String
    , secret :: String
    , path :: String
    , specialArgs :: Map String SpecialArg
    , aria2Secret :: Maybe Text
    }
  deriving (Generic)

instance FromJSON SpecialArg

instance FromJSON Config

data Model =
  Model
    { skipDeleted :: Bool
    , posts :: PointedList Post
    , subredditName :: R.SubredditName
    , downloaded :: Map R.PostID [FilePath]
    , downloadedMulti :: Map (R.PostID, Int) FilePath
    , downloadChannel :: Chan R.Post
    , slideshowThread :: Maybe ThreadId
    , multi :: Int
    , currentIndex :: Int
    , config :: Config
    }

save :: Model -> IO ()
save state@Model {subredditName = R.R subreddit, config} = do
  startTime <- getCPUTime
  writeFile
    (path config ++ Text.unpack subreddit ++ ".conf")
    (show $ posts state)
  endTime <- getCPUTime
  putStrLn $
    "Saved reddit in " ++ show ((endTime - startTime) `div` 1000000000) ++ "ms"

load :: Config -> String -> Chan R.Post -> IO (PointedList Post)
load config subr dlchannel =
  readEither <$>
  (readFile (path config ++ subr ++ ".conf") `catch` \(SomeException _) ->
     return "") >>= \case
    Left _ -> do
      initialPosts <- redditGet config (R.R $ Text.pack subr) Nothing dlchannel
      return
        PointedList
          {front = [], current = head initialPosts, back = tail initialPosts}
    Right r@PointedList {front = f, current = c, back = b} ->
      mapM_
        (\case
           Submitted p -> writeChan dlchannel p
           _ -> return ())
        (f ++ c : b) >>
      return r

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
    opts = R.Options {R.pagination = option, R.limit = Just 100}
    (redditOptions, redditAction) =
      case Map.lookup (Text.unpack arg) (specialArgs config) of
        Nothing ->
          ( R.defaultRedditOptions
              { R.customUserAgent =
                  Just (Text.encodeUtf8 $ Text.pack $ userAgent config)
              }
          , R.getPosts'
              R.Options {R.pagination = option, R.limit = Just 100}
              R.New
              (Just subreddit))
        Just SpecialArg {refreshToken, route} ->
          ( R.defaultRedditOptions
              { R.customUserAgent =
                  Just (Text.encodeUtf8 $ Text.pack $ userAgent config)
              , R.loginMethod =
                  R.OAuth
                    R.Script
                      { R.clientId = Text.pack $ clientId config
                      , R.secret = Text.pack $ secret config
                      , R.redirectUrl = ""
                      }
                    (R.RefreshToken $ Text.pack refreshToken)
              }
          , R.runRoute
              (Routes.Route
                 (map Text.pack route)
                 [ "limit" Routes.=. Options.limit opts
                 , "before" Routes.=. Options.before opts
                 , "after" Routes.=. Options.after opts
                 ]
                 "GET"))

next' :: PointedList a -> PointedList a
next' PointedList {front = f, current = c, back = h:t} =
  PointedList {front = c : f, current = h, back = t}

prev' :: PointedList a -> PointedList a
prev' PointedList {front = h:t, current = c, back = b} =
  PointedList {front = t, current = h, back = c : b}

front' :: PointedList a -> PointedList a
front' list@PointedList {front = []} = list
front' PointedList {front = f, current = c, back = b} =
  let h:t = reverse f
   in PointedList {front = [], current = h, back = t ++ c : b}

back' :: PointedList a -> PointedList a
back' list@PointedList {back = []} = list
back' PointedList {front = f, current = c, back = b} =
  let h:t = reverse b
   in PointedList {front = t ++ c : f, current = h, back = []}

refresh :: Model -> IO Model
refresh state@Model {posts = list@PointedList {current = c}} =
  case c of
    Downloaded p _ ->
      writeChan (downloadChannel state) p >> writeChan (downloadChannel state) p >>
      return state {posts = list {current = Submitted p}}
    Submitted _ -> return state -- do nothing, is already submitted
    Deleted i -> refresh' i
    Failed i -> refresh' i
  where
    refresh' :: R.PostID -> IO Model
    refresh' i =
      R.runRedditWith redditOptions (R.getPostInfo i) >>= \case
        Left msg ->
          putStrLn ("Failed to download " ++ show i) >>
          putStrLn ("Message: " ++ show msg) >>
          return state
        Right p ->
          case R.content p of
            R.Link _ ->
              writeChan (downloadChannel state) p >>
              return state {posts = list {current = Submitted p}}
            _ -> return state
    redditOptions :: R.RedditOptions
    redditOptions =
      R.defaultRedditOptions
        {R.customUserAgent = Just "Haskell reddit image viewer (by /u/rmanne)"}

next :: Model -> IO Model
next state@Model { skipDeleted = sd
                 , posts = list@PointedList {front = _, current = c, back = b}
                 } =
  case b of
    [] ->
      redditGet
        (config state)
        (subredditName state)
        (Just $ R.After $ postId c)
        (downloadChannel state) >>= \case
        [] -> return state
        b' -> next state {posts = list {back = b'}}
    Deleted _:_
      | sd -> next state {posts = next' list}
    _ -> return state {posts = next' list, currentIndex = 0}

nextUntilFailed :: Model -> IO Model
nextUntilFailed state =
  if isFailed (current (posts state)) ||
     all (not . isFailed) (back (posts state))
    then return state
    else next state >>= nextUntilFailed
  where
    isFailed (Failed _) = True
    isFailed _ = False

prev :: Model -> IO Model
prev state@Model { skipDeleted = sd
                 , posts = list@PointedList {front = f, current = c, back = b}
                 } =
  case f of
    [] ->
      redditGet
        (config state)
        (subredditName state)
        (Just $ R.Before $ postId c)
        (downloadChannel state) >>= \case
        [] ->
          case b of
            [] -> return state
            c':_ -- maybe c is deleted, try c'
             ->
              redditGet
                (config state)
                (subredditName state)
                (Just $ R.Before $ postId c')
                (downloadChannel state) >>= \case
                [] -> return state -- c' is deleted too...
                f'
                  | any (\c'' -> postId c == postId c'') f' ->
                    trace "Here" $ return state -- c isn't deleted, just return even if we can do better
                f' -> prev state {posts = list {front = reverse f'}}
        f' -> prev state {posts = list {front = reverse f'}}
    Deleted _:_
      | sd -> prev state {posts = prev' list}
    _ -> return state {posts = prev' list, currentIndex = 0}

slideshowWorker :: Chan Command -> IO ()
slideshowWorker channel =
  threadDelay 3000000 >> writeChan channel Next >> slideshowWorker channel

display :: IORef (Maybe View) -> Model -> IO Model
display ref state =
  writeIORef ref (Just (current $ posts state, currentIndex state)) >>
  return state

processAndDisplay :: IORef (Maybe View) -> Model -> IO Model
processAndDisplay ref state@Model {posts = list@PointedList {current = c}} =
  case c of
    (Submitted p) ->
      display ref $
      case Map.lookup (R.postID p) (downloaded state) of
        Nothing -> state
        Just fs ->
          state
            { posts =
                list
                  { current =
                      if null fs
                        then Failed (R.postID p)
                        else Downloaded p fs
                  }
            , downloaded = Map.delete (R.postID p) (downloaded state)
            }
    _ -> display ref state

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
  initialState <-
    processAndDisplay
      ref
      Model
        { posts = initialPosts
        , subredditName = R.R (Text.pack subr)
        , downloadChannel = dlchannel
        , skipDeleted = True
        , downloaded = Map.empty
        , downloadedMulti = Map.empty
        , slideshowThread = Nothing
        , multi = 1
        , currentIndex = 0
        , config = config
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
           Left msg -> print msg >> writeChan channel (Download pid index length [])
           Right f' ->
             renameFile (Text.unpack f') fname >>
             writeChan channel (Download pid index length [fname]))
  _ <- forkIO (downloadWorker config channel dlchannel aria2InputChan)
  _ <- forkIO (downloadWorker config channel dlchannel aria2InputChan)
  _ <- forkIO (downloadWorker config channel dlchannel aria2InputChan)
  _ <- forkIO (downloadWorker config channel dlchannel aria2InputChan)
  _ <- forkIO (downloadWorker config channel dlchannel aria2InputChan)
  _ <- forkIO (downloadWorker config channel dlchannel aria2InputChan)
  getChanContents channel >>=
    foldM_
      (\state ->
         \case
           Next ->
             foldM
               (\state' _ -> next state' {multi = 1})
               state
               [(1 :: Int) .. (multi state)] >>=
             processAndDisplay ref
           Prev ->
             foldM
               (\state' _ -> prev state' {multi = 1})
               state
               [(1 :: Int) .. (multi state)] >>=
             processAndDisplay ref
           Toggle ->
             case slideshowThread state of
               Nothing ->
                 forkIO (slideshowWorker channel) >>= \threadId ->
                   return state {slideshowThread = Just threadId, multi = 1}
               Just threadId ->
                 killThread threadId >>
                 return state {slideshowThread = Nothing, multi = 1}
           Remove ->
             case current (posts state) of
               Downloaded p f ->
                 mapM_ removeFile f `catch` (\SomeException {} -> return ()) >>
                 next
                   (state
                      { posts = (posts state) {current = Deleted (R.postID p)}
                      , multi = 1
                      }) >>=
                 processAndDisplay ref
               Failed p ->
                 next
                   (state
                      {posts = (posts state) {current = Deleted p}, multi = 1}) >>=
                 processAndDisplay ref
               _ -> return state {multi = 1}
           Save -> save state >> return state {multi = 1}
           ToggleDeleted ->
             return state {skipDeleted = not (skipDeleted state), multi = 1}
           Refresh -> refresh state {multi = 1, currentIndex = 0}
           Front ->
             processAndDisplay
               ref
               state {posts = front' (posts state), multi = 1, currentIndex = 0}
           Back ->
             processAndDisplay
               ref
               state {posts = back' (posts state), multi = 1, currentIndex = 0}
           Commit ->
             let Model { posts = PointedList {front = f, current = c, back = b}
                       , downloaded = dlmap
                       } = state
              in return
                   state
                     { posts =
                         PointedList
                           { front = map (fillPost dlmap) f
                           , current = fillPost dlmap c
                           , back = map (fillPost dlmap) b
                           }
                     , downloaded = Map.empty
                     , multi = 1
                     }
           FindFailed ->
             nextUntilFailed state {multi = 1} >>= processAndDisplay ref
           Status ->
             let Model { posts = PointedList {front = f, current = c, back = b}
                       , downloaded = dlmap
                       } = state
              in do putStrLn $ "Downloaded: " <> show dlmap
                    return state {multi = 1}
           NextImage ->
             processAndDisplay ref state {currentIndex = currentIndex state - 1}
           PrevImage ->
             processAndDisplay ref state {currentIndex = currentIndex state + 1}
           Multi num -> return state {multi = num}
           Download i 1 1 f ->
             case current (posts state) of
               Submitted p
                 | i == R.postID p ->
                   processAndDisplay
                     ref
                     state
                       { posts =
                           (posts state)
                             { current =
                                 if null f
                                   then Failed i
                                   else Downloaded p f
                             }
                       }
               _ ->
                 return state {downloaded = Map.insert i f (downloaded state)}
           Download postId index length f ->
             if all
                  id
                  (map
                     (\i' -> Map.member (postId, i') (downloadedMulti state))
                     (filter (/= index) [1 .. length]))
               then let [f'] = f
                        files =
                          map
                            (\i' ->
                               if i' == index
                                 then f'
                                 else (downloadedMulti state) Map.! (postId, i'))
                            [1 .. length]
                     in case current (posts state) of
                          Submitted p
                            | postId == R.postID p ->
                              processAndDisplay
                                ref
                                state
                                  { posts =
                                      (posts state)
                                        { current =
                                            if null files
                                              then Failed postId
                                              else Downloaded p files
                                        }
                                  }
                          _ ->
                            return
                              state
                                { downloaded =
                                    Map.insert postId files (downloaded state)
                                }
               else let [f'] = f
                     in return
                          state
                            { downloadedMulti =
                                Map.insert
                                  (postId, index)
                                  f'
                                  (downloadedMulti state)
                            })
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
            path config ++
            Text.unpack
              (let R.R subr = R.subreddit p
                in subr)
      createDirectoryIfMissing False dname
      forM_ (zip [1 ..] dl) $ \(i, dl') -> do
        let fname =
              if length dl == 1
                then dname ++ "/" ++ Text.unpack pid ++ takeExtension dl'
                else dname ++
                     "/" ++
                     Text.unpack pid ++ "-" ++ show i ++ takeExtension dl'
        doesFileExist fname >>= \case
          True -> do
            putStrLn $
              "File exists " ++ link ++ " (" ++ dl' ++ ") into " ++ fname
            writeChan channel (Download (R.postID p) i (length dl) [fname])
          False -> do
            putStrLn $
              "Downloading " ++ link ++ " (" ++ dl' ++ ") into " ++ fname
            writeChan
              aria2InputChan
              ((R.postID p, i, length dl, fname), Text.pack dl')
    _ -> do
      putStrLn $ "Unable to download " ++ link ++ " " ++ show dl
      writeChan channel (Download (R.postID p) 1 1 [])
downloadPost config channel aria2InputChan R.Post {R.postID = i} =
  writeChan channel (Download i 1 1 [])

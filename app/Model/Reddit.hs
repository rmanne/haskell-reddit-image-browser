module Model.Reddit
  ( list
  , post
  ) where

import Control.Lens ((%~), (&), _2)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Model.Types
  ( Config(clientId, secret, specialArgs, userAgent)
  , SpecialArg(SpecialArg, refreshToken, route)
  )
import qualified Network.API.Builder.Routes as Routes
import qualified Reddit as R
import qualified Reddit.Types.Listing as R
import qualified Reddit.Types.Options as Options
import qualified Reddit.Types.Post as R

post :: Config -> Text -> R.PostID -> IO (Maybe R.Post)
post config arg postId =
  R.runRedditWith (fst $ redditOptionsFor config arg) (R.getPostInfo postId) >>= \case
    Left msg ->
      putStrLn ("Failed to download " <> show postId) >>
      putStrLn ("Message: " <> show msg) >>
      return Nothing
    Right p -> return (Just p)

list :: Config -> Text -> Maybe (R.PaginationOption R.PostID) -> IO [R.Post]
list config arg option =
  R.runRedditWith redditOptions redditAction >>= \case
    Right (R.Listing before' after' contents) -> do
      print option
      print $ map R.postID contents
      print before'
      print after'
      return contents
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

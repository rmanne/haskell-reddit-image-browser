{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module ImageLink
  ( imageLinks,
    DownloadHandler (..),
  )
where

import Control.Exception (catch)
import Control.Lens ((^.))
import Control.Monad (foldM)
import Data.Aeson (FromJSON (parseJSON), Object, Value (Array, Object), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as HashMap
import Data.Aeson.Types (typeMismatch)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isSuffixOf)
import qualified Data.Vector as Vector
import Network.HTTP.Client (HttpException (HttpExceptionRequest))
import qualified Network.Wreq as Wreq
import System.Process (readCreateProcess, shell)
import Text.Regex.PCRE ((=~))

newtype RedditGallery = RedditGallery
  { redditUrls :: [String]
  }
  deriving (Show)

instance FromJSON RedditGallery where
  parseJSON (Array v) =
    ( case Vector.head v of
        Object o -> o .: "data"
        _ -> typeMismatch "Response should be an array of objects" (Array v)
    )
      >>= \d ->
        d .: "children" >>= \[c] ->
          c .: "data" >>= \d' ->
            d' .: "media_metadata" >>= \(mediaMetadata :: Object) ->
              mapM
                ( \(mediaId, Object itemDetails) ->
                    itemDetails .: "m" >>= \(itemType :: String) ->
                      case itemType of
                        "image/jpg" ->
                          return
                            ["https://i.redd.it/" ++ Key.toString mediaId ++ ".jpg"]
                        "image/png" ->
                          return
                            ["https://i.redd.it/" ++ Key.toString mediaId ++ ".png"]
                        "image/gif" ->
                          return
                            ["https://i.redd.it/" ++ Key.toString mediaId ++ ".gif"]
                        _ -> return []
                )
                (HashMap.toList mediaMetadata)
                >>= \urls ->
                  return (RedditGallery (concat urls))
  parseJSON v = typeMismatch "Response should be an aeson object" v

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

imageLinks :: Handler
imageLinks link =
  foldM
    ( \links handler ->
        if null links
          then
            filter (not . null)
              <$> ( handler link `catch` \e ->
                      putStrLn
                        ( "Failed "
                            ++ show link
                            ++ "; Caught "
                            ++ show (e :: HttpException)
                        )
                        >> return []
                  )
          else return links
    )
    []
    handlers

data DownloadHandler = YoutubeDL | DirectLink deriving (Show)

type Handler = String -> IO [(String, DownloadHandler)]

type DirectLinkHandler = String -> IO [String]

handlers :: [Handler]
handlers =
  [ fmap (map (,DirectLink)) . redditPreviewHandler,
    fmap (map (,YoutubeDL)) . redgifImageHandler, -- sometimes with a .jpg suffix
    fmap (map (,DirectLink)) . imageHandler,
    fmap (map (,DirectLink)) . baseImageHandler,
    fmap (map (,DirectLink)) . gifvHandler,
    fmap (map (,DirectLink)) . imgurImageHandler,
    fmap (map (,DirectLink)) . imgurAlbumHandler,
    fmap (map (,DirectLink)) . imgurGalleryHandler,
    fmap (map (,DirectLink)) . imgurOtherHandler,
    fmap (map (,YoutubeDL)) . gfycatHandler,
    fmap (map (,YoutubeDL)) . redgifHandler,
    fmap (map (,DirectLink)) . redditGalleryHandler,
    fmap (map (,YoutubeDL)) . redditVideoHandler,
    fmap (map (,YoutubeDL)) . xvideosHandler
  ]

redditGalleryHandler :: DirectLinkHandler
redditGalleryHandler link =
  case link =~ ("^https?://(www.|)reddit.com/gallery/([a-zA-Z0-9]*)$" :: String) :: [[String]] of
    [[_, _, redditId]] ->
      ( \case
          Nothing -> []
          Just x -> x
      )
        . fmap redditUrls
        . Aeson.decode
        . (^. Wreq.responseBody)
        <$> Wreq.get ("https://www.reddit.com/comments/" ++ redditId ++ ".json")
    _ -> return []

imageHandler :: DirectLinkHandler
imageHandler link =
  if isSuffixOf ".jpg" link
    || isSuffixOf ".jpgtrue" link
    || isSuffixOf ".jpeg" link
    || isSuffixOf ".png" link
    || isSuffixOf ".webp" link
    || isSuffixOf ".webm" link
    || isSuffixOf ".gif" link
    || isSuffixOf ".mp4" link
    then return [link]
    else return []

baseImageHandler :: DirectLinkHandler
baseImageHandler link =
  case link =~ ("^(.*)\\?[^?]*$" :: String) :: [[String]] of
    [[_, baseUrl]] ->
      imageHandler baseUrl >>= \case
        [] -> return []
        _ -> return [link]
    _ -> return []

gifvHandler :: DirectLinkHandler
gifvHandler link =
  if ".gifv" `isSuffixOf` link
    then return [take (length link - 4) link ++ "mp4"]
    else return []

imgurImageHandler :: DirectLinkHandler
imgurImageHandler link = do
  case link =~ ("^https?://(www.)?imgur.com/[a-zA-Z0-9]*$" :: String) :: [[String]] of
    [_] ->
      (: []) . trim
        <$> readCreateProcess
          ( shell $
              "curl -L -s '"
                ++ link
                ++ "' | sed -n 's/.*image_src.*href=\"\\(.*\\)\".*/\\1/p'"
          )
          ""
    _ -> return []

imgurAlbumHandler :: DirectLinkHandler
imgurAlbumHandler link =
  case link =~ ("^https?://(www.)?imgur.com/a/[a-zA-Z0-9]*$" :: String) :: [[String]] of
    [_] -> do
      putStrLn "Found imgur album, not yet fully implemented"
      link' <-
        trim
          <$> readCreateProcess
            ( shell $
                "curl -L -s '"
                  <> link
                  <> "' | sed -n -e 's/.*image_src.*href=\"\\([^\"]*\\)\".*/\\1/p' -e 's/.*twitter:player:stream.*content=\"\\([^\"]*\\.mp4\\)\".*/\\1/p'"
            )
            ""
      return [link']
    _ -> return []

imgurGalleryHandler :: DirectLinkHandler
imgurGalleryHandler link =
  case link =~ ("^https?://(www.)?imgur.com/gallery/[a-zA-Z0-9]*$" :: String) :: [[String]] of
    [_] -> do
      putStrLn "Found imgur gallery, not yet fully implemented"
      link' <-
        trim
          <$> readCreateProcess
            ( shell $
                "curl -L -s '"
                  ++ link
                  ++ "' | sed -n 's/.*image_src.*href=\"\\(.*\\)\".*/\\1/p'"
            )
            ""
      return [link']
    _ -> return []

imgurOtherHandler :: DirectLinkHandler
imgurOtherHandler link =
  case link =~ ("^https?://(www.|m.)?imgur.com/.*$" :: String) :: [[String]] of
    [_] -> do
      putStrLn "Found other imgur link"
      let link' =
            case link =~ ("(.*)\\?[^?]*" :: String) :: [[String]] of
              [[_, baseUrl]] -> baseUrl
              _ -> link
      let imageLink1 =
            trim
              <$> readCreateProcess
                ( shell $
                    "curl -L -s '"
                      ++ link'
                      ++ "' | sed -n 's/.*contentURL.*content=\"\\(.*\\)\".*/\\1/p'"
                )
                ""
      let imageLink2 =
            trim
              <$> readCreateProcess
                ( shell $
                    "curl -L -s '"
                      ++ link'
                      ++ "' | sed -n 's/.*image_src.*href=\"\\(.*\\)\".*/\\1/p'"
                )
                ""
      let imageLink3 =
            trim
              <$> readCreateProcess
                ( shell $
                    "curl -L -s '"
                      ++ link'
                      ++ "' | grep -wo 'https://i.imgur[^\"]*' | grep -v '?'"
                )
                ""
      (: []) <$> (imageLink1 <|> imageLink2 <|> imageLink3)
    _ -> return []
  where
    (<|>) :: IO String -> IO String -> IO String
    a <|> b =
      a >>= \case
        "" -> b
        s -> return s

gfycatHandler :: DirectLinkHandler
gfycatHandler link =
  case link
         =~ ("^https?://(www.|)gfycat.com(/..|/gifs/detail)?/([a-zA-Z]*)(-.*)?/?(\\?.*)?$" :: String) ::
         [[String]] of
    --[[_, _, _, gfycatId, _, _]] -> return ["https://www.gfycat.com/" <> gfycatId]
    [[_, _, _, gfycatId, _, _]] -> return ["https://www.redgifs.com/watch/" <> gfycatId]
    _ -> return []

redgifImageHandler :: DirectLinkHandler
redgifImageHandler link =
  case link =~ ("^https?://i.redgifs.com/i/([a-zA-Z0-9]*)(#rel=.*|\\?.*|.jpg)$" :: String) :: [[String]] of
    [[_, gfycatId, _]] -> return ["https://www.redgifs.com/watch/" <> gfycatId]
    _ -> return []

redgifHandler :: DirectLinkHandler
redgifHandler link =
  case link =~ ("^https?://(www.|v3.|)redgifs.com/(watch|ifr)/([a-zA-Z0-9]*)(#rel=.*|\\?.*|)$" :: String) :: [[String]] of
    [[_, _, _, gfycatId, _]] -> return ["https://www.redgifs.com/watch/" <> gfycatId]
    _ -> return []

redditVideoHandler :: DirectLinkHandler
redditVideoHandler link =
  case link =~ ("^https://v.redd.it/([a-zA-Z0-9]*)$" :: String) :: [[String]] of
    [[_, _]] -> return [link]
    _ -> return []

redditPreviewHandler :: DirectLinkHandler
redditPreviewHandler link =
  case link =~ ("^https://preview.redd.it/([a-zA-Z0-9]*)(.png|.jpg)(\\?.*)?$" :: String) :: [[String]] of
    [[_, redditImageId, extension, _]] -> return ["https://i.redd.it/" <> redditImageId <> extension]
    _ -> return []

xvideosHandler :: DirectLinkHandler
xvideosHandler link =
  -- https://github.com/ytdl-org/youtube-dl/issues/30271
  case link =~ ("^https?://www.xvideos.com/([a-zA-Z0-9]*)/.*$" :: String) :: [[String]] of
    [[_, _]] -> return [link]
    _ -> return []

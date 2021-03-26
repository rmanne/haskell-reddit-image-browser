{-# LANGUAGE ScopedTypeVariables #-}

module ImageLink
  ( imageLinks
  ) where

import Control.Exception (IOException, catch, throw)
import Control.Lens ((^.))
import Control.Monad (foldM)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON(parseJSON), Object, Value(Array, Object), (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Char (isSpace)
import qualified Data.HashMap.Strict as HashMap
import Data.List (dropWhileEnd, isSuffixOf)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Network.HTTP.Client
  ( HttpException(HttpExceptionRequest)
  , HttpExceptionContent(StatusCodeException)
  )
import qualified Network.Wreq as Wreq
import System.Process (readCreateProcess, shell)
import Text.Regex.PCRE ((=~))

newtype Gfycat =
  Gfycat
    { mp4Url :: String
    }
  deriving (Show)

instance FromJSON Gfycat where
  parseJSON (Object v) = v .: "gfyItem" >>= \o -> Gfycat <$> o .: "mp4Url"
  parseJSON v = typeMismatch "Gfycat response should be an aeson object" v

newtype RedditGallery =
  RedditGallery
    { redditUrls :: [String]
    }
  deriving (Show)

instance FromJSON RedditGallery where
  parseJSON (Array v) =
    (case Vector.head v of
       Object o -> o .: "data"
       _ -> typeMismatch "Response should be an array of objects" (Array v)) >>= \d ->
      d .: "children" >>= \[c] ->
        c .: "data" >>= \d' ->
          d' .: "media_metadata" >>= \(mediaMetadata :: Object) ->
            mapM
              (\(mediaId, Object itemDetails) ->
                 itemDetails .: "m" >>= \(itemType :: String) ->
                   case itemType of
                     "image/jpg" ->
                       return
                         ["https://i.redd.it/" ++ Text.unpack mediaId ++ ".jpg"]
                     "image/png" ->
                       return
                         ["https://i.redd.it/" ++ Text.unpack mediaId ++ ".png"]
                     "image/gif" ->
                       return
                         ["https://i.redd.it/" ++ Text.unpack mediaId ++ ".gif"]
                     _ -> return [])
              (HashMap.toList mediaMetadata) >>= \urls ->
              return (RedditGallery (concat urls))
  parseJSON v = typeMismatch "Response should be an aeson object" v

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

imageLinks :: String -> IO [String]
imageLinks link =
  foldM
    (\links handler ->
       if null links
         then filter (not . null) <$>
              (handler link `catch` \e ->
                 putStrLn
                   ("Failed " ++
                    link ++ "; Caught " ++ show (e :: HttpException)) >>
                 return [])
         else return links)
    []
    handlers

type Handler = String -> IO [String]

handlers :: [Handler]
handlers =
  [ imageHandler
  , baseImageHandler
  , gifvHandler
  , imgurImageHandler
  , imgurAlbumHandler
  , imgurGalleryHandler
  , imgurOtherHandler
  , gfycatHandler
  , redgifHandler
  , redditGalleryHandler
  ]

redditGalleryHandler :: Handler
redditGalleryHandler link =
  case link =~ ("https?://(www.|)reddit.com/gallery/([a-zA-Z0-9]*)" :: String) :: [[String]] of
    [[_, _, redditId]] ->
      (\case
         Nothing -> []
         Just x -> x) .
      fmap redditUrls . Aeson.decode . (^. Wreq.responseBody) <$>
      Wreq.get ("https://www.reddit.com/comments/" ++ redditId ++ ".json")
    _ -> return []

imageHandler :: Handler
imageHandler link =
  if isSuffixOf ".jpg" link ||
     isSuffixOf ".jpgtrue" link ||
     isSuffixOf ".jpeg" link ||
     isSuffixOf ".png" link ||
     isSuffixOf ".webp" link ||
     isSuffixOf ".webm" link || isSuffixOf ".gif" link || isSuffixOf ".mp4" link
    then return [link]
    else return []

baseImageHandler :: Handler
baseImageHandler link =
  case link =~ ("(.*)\\?[^?]*" :: String) :: [[String]] of
    [[_, baseUrl]] ->
      imageHandler baseUrl >>= \case
        [] -> return []
        _ -> return [link]
    _ -> return []

gifvHandler :: Handler
gifvHandler link =
  if ".gifv" `isSuffixOf` link
    then return [take (length link - 4) link ++ "mp4"]
    else return []

imgurImageHandler :: Handler
imgurImageHandler link = do
  case link =~ ("https?://(www.)?imgur.com/[a-zA-Z0-9]*" :: String) :: [[String]] of
    [_] ->
      (: []) . trim <$>
      readCreateProcess
        (shell $
         "curl -L -s '" ++
         link ++ "' | sed -n 's/.*image_src.*href=\"\\(.*\\)\".*/\\1/p'")
        ""
    _ -> return []

imgurAlbumHandler :: Handler
imgurAlbumHandler link =
  case link =~ ("https?://(www.)?imgur.com/a/[a-zA-Z0-9]*" :: String) :: [[String]] of
    [_] -> do
      putStrLn "Found imgur album, not yet fully implemented"
      link' <-
        trim <$>
        readCreateProcess
          (shell $
           "curl -L -s '" ++
           link ++ "' | sed -n 's/.*image_src.*href=\"\\([^\"]*\\)\".*/\\1/p'")
          ""
      return [link']
    _ -> return []

imgurGalleryHandler :: Handler
imgurGalleryHandler link =
  case link =~ ("https?://(www.)?imgur.com/gallery/[a-zA-Z0-9]*" :: String) :: [[String]] of
    [_] -> do
      putStrLn "Found imgur gallery, not yet fully implemented"
      link' <-
        trim <$>
        readCreateProcess
          (shell $
           "curl -L -s '" ++
           link ++ "' | sed -n 's/.*image_src.*href=\"\\(.*\\)\".*/\\1/p'")
          ""
      return [link']
    _ -> return []

imgurOtherHandler :: Handler
imgurOtherHandler link =
  case link =~ ("https?://(www.|m.)?imgur.com/.*" :: String) :: [[String]] of
    [_] -> do
      putStrLn "Found other imgur link"
      let link' =
            case link =~ ("(.*)\\?[^?]*" :: String) :: [[String]] of
              [[_, baseUrl]] -> baseUrl
              _ -> link
      let imageLink1 =
            trim <$>
            readCreateProcess
              (shell $
               "curl -L -s '" ++
               link' ++
               "' | sed -n 's/.*contentURL.*content=\"\\(.*\\)\".*/\\1/p'")
              ""
      let imageLink2 =
            trim <$>
            readCreateProcess
              (shell $
               "curl -L -s '" ++
               link' ++ "' | sed -n 's/.*image_src.*href=\"\\(.*\\)\".*/\\1/p'")
              ""
      let imageLink3 =
            trim <$>
            readCreateProcess
              (shell $
               "curl -L -s '" ++
               link' ++ "' | grep -wo 'https://i.imgur[^\"]*' | grep -v '?'")
              ""
      (: []) <$> (imageLink1 <|> imageLink2 <|> imageLink3)
    _ -> return []
  where
    (<|>) :: IO String -> IO String -> IO String
    a <|> b =
      a >>= \case
        "" -> b
        s -> return s

gfycatHandler :: Handler
gfycatHandler link =
  case link =~
       ("^https?://(www.|)gfycat.com(/..|/gifs/detail)?/([a-zA-Z]*)(-.*)?/?(\\?.*)?$" :: String) :: [[String]] of
    [[_, _, _, gfycatId, _, _]] -> withGfycatId gfycatId
    _ -> return []

redgifHandler :: Handler
redgifHandler link =
  case link =~ ("https?://(www.|)redgifs.com/watch/([a-zA-Z]*)" :: String) :: [[String]] of
    [[_, _, gfycatId]] -> withGfycatId gfycatId
    _ -> return []
  where
    fallback :: String -> HttpException -> IO [String]
    fallback gfycatId _ =
      (\newId -> ["https://thumbs1.redgifs.com/" <> newId <> ".webm"]) . trim <$>
      readCreateProcess
        (shell $
         "curl -L -s '" <>
         link <> "' | sed -n 's#.*watch/\\([^\"]*\\).*#\\1#p' | head -n1")
        ""

withGfycatId :: String -> IO [String]
withGfycatId gfycatId =
  (do putStrLn $ "Gfycat ID: " ++ gfycatId
      (\case
         Nothing -> []
         Just x -> [x]) .
        fmap mp4Url . Aeson.decode . (^. Wreq.responseBody) <$>
        Wreq.get ("https://api.gfycat.com/v1/gfycats/" ++ gfycatId)) `catch`
  (\e ->
     case e of
       HttpExceptionRequest _ (StatusCodeException _ _) ->
         withRedgifsId gfycatId
       _ -> throw e)

withRedgifsId :: String -> IO [String]
withRedgifsId redgifsId = do
  putStrLn $ "Redgifs ID: " ++ redgifsId
  (\case
     Nothing -> []
     Just x -> [x]) .
    fmap mp4Url . Aeson.decode . (^. Wreq.responseBody) <$>
    Wreq.get ("https://api.redgifs.com/v1/gfycats/" ++ redgifsId)

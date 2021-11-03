{-# LANGUAGE DeriveGeneric #-}

module Network.Aria2
  ( run
  , secret
  , host
  , port
  , uri
  , onLibraryError
  , defaultOptions
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, getChanContents, writeChan)
import Control.Monad (forever, join, mapM, mapM_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Control.Monad.Trans.State.Lazy as State
import Data.Aeson (FromJSON, ToJSON(toJSON), Value, eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (chr)
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding as Text
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Network.HTTP.Conduit
  ( Manager
  , defaultRequest
  , httpLbs
  , newManager
  , tlsManagerSettings
  )
import qualified Network.HTTP.Conduit as HTTP
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import Prelude hiding (id)

type GID = Text

data Options =
  Options
    { secret :: Maybe Text
    , host :: Maybe Text
    , port :: Maybe Int
    , resource :: Maybe Text
    , onLibraryError :: Text -> IO ()
    }

defaultOptions :: Options
defaultOptions =
  Options
    { secret = Nothing
    , host = Nothing
    , port = Nothing
    , resource = Nothing
    , onLibraryError = \_ -> return ()
    }

-- (Some ID, URL) -> (URL, PATH)
app ::
     Options -> Chan (a, Text) -> Chan (a, Either Text Text) -> WS.ClientApp ()
app Options {..} inputChan outputChan conn = do
  urlToCustomId <- newIORef (Map.empty :: Map Text a)
  let processNotification :: Notification -> IO ()
      processNotification Notification {method = m, params = [params]}
        | m == "aria2.onDownloadComplete" ||
            m == "aria2.onDownloadError" || m == "aria2.onDownloadStop" =
          Text.pack . show <$> nextRandom >>= \uuid ->
            WS.sendTextData
              conn
              ("{\"jsonrpc\":\"2.0\",\"id\":\"" <>
               uuid <>
               "\",\"method\":\"aria2.tellStatus\",\"params\":[" <>
               (case secret of
                  Nothing -> ""
                  Just v -> "\"token:" <> v <> "\",") <>
               Text.decodeUtf8 (LBS.toStrict (encode (gid params))) <> "]}")
      processNotification _ = return ()
  let processAddUri :: RPCResponse GID -> IO ()
      processAddUri _ = return ()
  let processStatus :: RPCResponse Status -> IO ()
      processStatus RPCResponse {result = Status { status = "complete"
                                                 , files = [File { path = p
                                                                 , uris = Uri {uri = u}:_
                                                                 }]
                                                 }} =
        readIORef urlToCustomId >>= \m ->
          case Map.lookup u m of
            Just x -> writeChan outputChan (x, Right p)
            Nothing -> return ()
      processStatus RPCResponse {result = Status { status = s
                                                 , errorMessage = e
                                                 , files = [File { path = p
                                                                 , uris = Uri {uri = u}:_
                                                                 }]
                                                 }}
        | s == "error" || s == "removed" =
          readIORef urlToCustomId >>= \m ->
            case Map.lookup u m of
              Just x -> writeChan outputChan (x, Left e)
              Nothing -> return ()
      processStatus somethingElse =
        onLibraryError (Text.pack (show somethingElse))
  -- 1 thread reads from aria2 (notifications included) and writes to outputChan
  forkIO $
    forever $ do
      msg <- WS.receiveData conn
      case eitherDecode msg of
        Right val -> processNotification val
        Left err1 ->
          case eitherDecode msg of
            Right val -> processStatus val
            Left err2 ->
              case eitherDecode msg of
                Right val -> processAddUri val
                Left err3 -> onLibraryError (Text.decodeUtf8 (LBS.toStrict msg))
  -- main thread reads from inputChan and writes it to aria2
  getChanContents inputChan >>=
    mapM_
      (\(customId, url) ->
         Text.pack . show <$> nextRandom >>= \uuid ->
           atomicModifyIORef
             urlToCustomId
             (\m -> (Map.insert url customId m, ())) >>
           WS.sendTextData
             conn
             ("{\"jsonrpc\":\"2.0\",\"id\":\"" <>
              uuid <>
              "\",\"method\":\"aria2.addUri\",\"params\":[" <>
              (case secret of
                 Nothing -> ""
                 Just v -> "\"token:" <> v <> "\",") <>
              Text.decodeUtf8 (LBS.toStrict (encode [url])) <> "]}"))
  WS.sendClose conn ("Bye!" :: Text)

run :: Options -> Chan (a, Text) -> Chan (a, Either Text Text) -> IO ()
run options@Options {..} inputChan outputChan =
  withSocketsDo $
  WS.runClient
    (maybe "127.0.0.1" Text.unpack host)
    (fromMaybe 6800 port)
    (maybe "/jsonrpc" Text.unpack resource) $
  app options inputChan outputChan

data RPCResponse a =
  RPCResponse
    { jsonrpc :: Text
    , id :: Text
    , result :: a
    }
  deriving (Generic, Show)

instance FromJSON a => FromJSON (RPCResponse a)

data File =
  File
    { path :: Text
    , uris :: [Uri]
    }
  deriving (Generic, Show)

instance FromJSON File

data Uri =
  Uri
    { uri :: Text
    }
  deriving (Generic, Show)

instance FromJSON Uri

data Status =
  Status
    { status :: Text
    , errorMessage :: Text
    , files :: [File]
    }
  deriving (Generic, Show)

instance FromJSON Status

data Notification =
  Notification
    { method :: Text
    , params :: [NotificationParams]
    }
  deriving (Generic, Show)

instance FromJSON Notification

data NotificationParams =
  NotificationParams
    { gid :: Text
    }
  deriving (Generic, Show)

instance FromJSON NotificationParams

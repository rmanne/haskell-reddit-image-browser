module Model.Config
  ( save
  , load
  ) where

import Control.Concurrent.Chan (Chan, writeChan)
import Control.Exception (Exception, catch, throw)
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.PointedList (PointedList, fromList)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Model.Reddit as Reddit
import Model.Types (Config(path))
import qualified OldPointedList
import qualified Reddit.Types.Post as R
import System.Directory (doesFileExist)
import System.Time.Extra (duration, showDuration)
import Text.Read (readEither)
import Types (Post(Submitted))

save :: MonadIO m => Config -> Text -> PointedList Post -> m ()
save config subr posts =
  liftIO (duration (writeFile fileName (show posts))) >>=
  liftIO . putStrLn . ("Saved reddit in " <>) . showDuration . fst
  where
    fileName = Text.unpack (path config) <> Text.unpack subr <> ".conf"

load :: Config -> Text -> Chan R.Post -> IO (PointedList Post)
load config subr dlchannel =
  ifM (doesFileExist fileName) loadFile loadNetwork >>= (<$) <*> sendDownload
  where
    fileName = Text.unpack (path config) <> Text.unpack subr <> ".conf"
    loadNetwork :: IO (PointedList Post)
    loadNetwork = fromList . fmap Submitted <$> Reddit.list config subr Nothing
    loadFile :: IO (PointedList Post)
    loadFile =
      readFromFile `catch`
      (\p1@ParseException {} ->
         OldPointedList.conv <$> readFromFile `catch` (\p2 -> throw (p1 <> p2)))
    sendDownload :: Foldable f => f Post -> IO ()
    sendDownload =
      mapM_
        (\case
           Submitted post -> writeChan dlchannel post
           _ -> return ())
    readFromFile :: (Read a, MonadIO m) => m a
    readFromFile =
      either (throw . ParseException) id . readEither <$>
      liftIO (readFile fileName)

newtype ParseException =
  ParseException
    { message :: String
    }
  deriving (Show)

instance Semigroup ParseException where
  p1 <> p2 = ParseException (message p1 <> "; " <> message p2)

instance Exception ParseException

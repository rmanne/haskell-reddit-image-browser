module Lib
  ( download
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Simple
import Network.HTTP.Conduit(simpleHttp)
import System.Process


download :: String -> FilePath -> IO ()
download url filepath =
  parseRequest url >>= httpBS >>= BS.writeFile filepath . getResponseBody

--download :: String -> FilePath -> IO ()
--download url filepath =
--  simpleHttp url >>= LBS.writeFile filepath

--download url filepath =
--  readCreateProcess
--    (shell $ "curl -m 60 -Lks '" ++ url ++ "' -o '" ++ filepath ++ "'")
--    ""
  --parseRequest url >>= httpBS >>= BS.writeFile filepath . getResponseBody

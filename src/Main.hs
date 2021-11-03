module Main
  ( main
  ) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Chan (newChan)
import Control.Exception (finally)
import Model (model)
import System.Environment (getArgs)
import View (view)

main :: IO ()
main = do
  [subr] <- getArgs
  modelChannel <- newChan
  viewChannel <- newChan
  modelWaiter <- newEmptyMVar
  _ <-
    forkIO $
    model subr modelChannel viewChannel `finally` putMVar modelWaiter ()
  view modelChannel viewChannel
  takeMVar modelWaiter

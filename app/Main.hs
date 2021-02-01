module Main
  ( main
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan)
import Model (model)
import System.Environment (getArgs)
import View (view)

main :: IO ()
main = do
  [subr] <- getArgs
  modelChannel <- newChan
  viewChannel <- newChan
  _ <- forkIO $ model subr modelChannel viewChannel
  view modelChannel viewChannel

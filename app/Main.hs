module Main where

import View(view)
import Model (model)
import System.Environment (getArgs)
import Types
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Control.Concurrent(forkIO)
import Control.Concurrent.Chan(newChan)

main :: IO ()
main = do
  [subr] <- getArgs
  modelChannel <- newChan
  viewChannel <- newChan
  _ <- forkIO $ model subr modelChannel viewChannel
  view modelChannel viewChannel

module Main2 where

import qualified ViewSDL
import Model (model)
import System.Environment (getArgs)
import Types
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Control.Concurrent(forkIO)
import Control.Concurrent.Chan(newChan)

main :: IO ()
main = do
  [subr] <- getArgs
  viewRef <- newIORef Nothing
  requestChannel <- newChan
  _ <- forkIO $ model subr viewRef requestChannel
  ViewSDL.view requestChannel viewRef

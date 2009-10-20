module FRP.Peakachu
  ( runProgram
  ) where

import FRP.Peakachu.Backend (Backend(..), Sink(..))
import FRP.Peakachu.Program (Program(..))
import Control.Concurrent.MVar.YC (writeMVar)

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (when)
import Data.Function (fix)
import Data.Maybe (isNothing)

doWhile :: Monad m => m Bool -> m ()
doWhile x = fix $ (x >>=) . flip when

runProgram :: Backend o i -> Program i o -> IO ()
runProgram backend program = do
  progVar <- newMVar program
  resumeVar <- newMVar True
  sinkVar <- newMVar Nothing
  let
    consumeOutput =
      doWhile $ do
        Just sink <- readMVar sinkVar
        prog <- takeMVar progVar
        case progVals prog of
          [] -> do
            putMVar progVar prog
            when (isNothing (progMore prog)) $ do
              sinkQuitLoop sink
              writeMVar resumeVar False
            return False
          (x : xs) -> do
            putMVar progVar prog { progVals = xs }
            sinkConsume sink x
            return True
    handleInput val = do
      prog <- takeMVar progVar
      let
        Just more = progMore prog
        m = more val
      putMVar progVar $ m { progVals = progVals prog ++ progVals m }
      consumeOutput
  sink <- runBackend backend handleInput
  writeMVar sinkVar (Just sink)
  -- consumeOutput
  case sinkMainLoop sink of
    Nothing ->
      doWhile $ do
        threadDelay 200000 -- 0.2 sec
        readMVar resumeVar
    Just mainloop -> mainloop


module FRP.Peakachu
  ( runProgram
  ) where

import FRP.Peakachu.Backend (Backend(..), Sink(..))
import FRP.Peakachu.Program (Program(..))
import Control.Concurrent.MVar.YC (writeMVar)
import Data.Cons
import Data.InfiniteStream

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newMVar, putMVar, readMVar, takeMVar)
import Control.Compose
import Control.Monad (when)
import Data.Function (fix)

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
        prog@(Prog (Cons vals more)) <- takeMVar progVar
        case vals of
          [] -> do
            putMVar progVar prog
            --when (isNothing (progMore prog)) $ do
            --  sinkQuitLoop sink
            --  writeMVar resumeVar False
            return False
          (x : xs) -> do
            putMVar progVar . Prog $ Cons xs more
            sinkConsume sink x
            return True
    handleInput val = do
      Prog (Cons vals more) <- takeMVar progVar
      let
        Just jMore = unO . runInfStrT $ more
        Cons mVals mMore = jMore val
      putMVar progVar . Prog $ Cons (vals ++ mVals) mMore
      consumeOutput
  sink <- runBackend backend handleInput
  writeMVar sinkVar (Just sink)
  sinkInit sink
  forkIO $ do
    threadDelay 300000
    consumeOutput
  case sinkMainLoop sink of
    Nothing ->
      doWhile $ do
        threadDelay 200000 -- 0.2 sec
        readMVar resumeVar
    Just mainloop -> mainloop


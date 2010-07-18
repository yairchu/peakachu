module FRP.Peakachu
    ( processList, processListV, runProgram
    ) where

import FRP.Peakachu.Backend (Backend (..))
import FRP.Peakachu.Backend.Internal (Sink (..), MainLoop (..), ParallelIO (..))
import FRP.Peakachu.Program (Program (..))
import Control.Concurrent.MVar.YC (writeMVar)

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (liftM, when)
import Control.Monad.Trans.List.Funcs (repeatM)
import Data.List.Class (List, concat, execute, scanl, takeWhile)
import Data.Maybe (isNothing)
import Prelude hiding (concat, scanl, takeWhile)

-- | "Verbose" version of 'processList'.
--
-- The program's outputs after each input are grouped together
processListV :: List l => Program a b -> l a -> l [b]
processListV program
    = liftM (progVals . snd) . takeWhile fst . scanl step (True, program)
    where
        step (_, Program _ Nothing) _ = (False, Program [] Nothing)
        step (_, Program _ (Just more)) x = (True, more x)

processList :: List l => Program a b -> l a -> l b
processList program = concat . processListV program

doWhile :: Monad m => m Bool -> m ()
doWhile = execute . takeWhile id . repeatM

runProgram :: Backend o i -> Program i o -> IO ()
runProgram backend program = do
    progVar <- newMVar program
    resumeVar <- newMVar True
    sinkVar <- newMVar Nothing
    let
        consumeOutput =
            doWhile $ do
                Just sink <- readMVar sinkVar
                prog@(Program vals more) <- takeMVar progVar
                case vals of
                    [] -> do
                        putMVar progVar prog
                        when (isNothing (progMore prog)) $ do
                            mlQuit $ sinkMainLoop sink
                            writeMVar resumeVar False
                        return False
                    (x : xs) -> do
                        putMVar progVar $ Program xs more
                        sinkConsume sink x
                        return True
        handleInput val = do
            prog@(Program vals maybeMore) <- takeMVar progVar
            case maybeMore of
                Nothing ->
                    putMVar progVar prog
                Just more -> do
                    let Program mVals mMore = more val
                    putMVar progVar $ Program (vals ++ mVals) mMore
                    consumeOutput
    sink <- runBackend backend handleInput
    writeMVar sinkVar (Just sink)
    mlInit $ sinkMainLoop sink
    _ <- forkIO $ do
        threadDelay 300000
        consumeOutput
    case mlRun (sinkMainLoop sink) of
        Nothing ->
            doWhile $ do
                threadDelay 200000 -- 0.2 sec
                readMVar resumeVar
        Just mainloop -> runParIO mainloop

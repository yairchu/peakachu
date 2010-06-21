-- | A Peakachu backend to write output to the console

module FRP.Peakachu.Backend.StdIO
    ( stdoutB, interactB
    ) where

import FRP.Peakachu.Backend (Backend(..))
import FRP.Peakachu.Backend.Internal (Sink(..), MainLoop(..), ParallelIO(..))
import Control.Concurrent.MVar.YC (writeMVar)

import Control.Concurrent.MVar
    (newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (when)
import Data.Monoid (mempty)
import System.IO (hFlush, hReady, stdin, stdout)

stdoutB :: Backend String ()
stdoutB =
    Backend . return . return $
    mempty { sinkConsume = (>> hFlush stdout) . putStr }

whileM :: Monad m => m Bool -> m () -> m ()
whileM cond iter = do
    resume <- cond
    when resume $ do
        iter
        whileM cond iter

-- | The Peakachu equivalent to 'interact'.
-- Prints all output lines from the program, and feeds
-- input lines from the user to the program.
interactB :: Backend String String
interactB =
    Backend f
    where
        f handler = do
            resumeVar <- newMVar True
            lineVar <- newMVar ""
            return Sink
                { sinkConsume = putStrLn
                , sinkMainLoop =
                        mempty
                        { mlQuit = writeMVar resumeVar False
                        , mlRun =
                                Just . ParIO . whileM (readMVar resumeVar) $ do
                                    isReady <- hReady stdin
                                    when isReady $ do
                                        c <- getChar
                                        prevLine <- takeMVar lineVar
                                        case c of
                                            '\n' -> do
                                                _ <- handler prevLine
                                                putMVar lineVar ""
                                            _ ->
                                                putMVar lineVar $ prevLine ++ [c]
                        }
                }


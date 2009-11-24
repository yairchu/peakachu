-- | A Peakachu backend to write output to the console

module FRP.Peakachu.Backend.StdIO
  ( stdoutB, interactB
  ) where

import FRP.Peakachu.Backend (Backend(..), Sink(..))
import Control.Concurrent.MVar.YC (writeMVar)

import Control.Concurrent.MVar (newMVar, readMVar)
import Control.Monad (when)
import Data.Monoid (mempty)
import System.IO (hFlush, stdout)

outSink :: Sink String
outSink = mempty { sinkConsume = (>> hFlush stdout) . putStr }

stdoutB :: Backend String ()
stdoutB = Backend . return . return $ outSink

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
      return outSink
        { sinkQuitLoop = writeMVar resumeVar False
        , sinkMainLoop =
            Just $ whileM
            (readMVar resumeVar)
            (getLine >>= handler)
        }


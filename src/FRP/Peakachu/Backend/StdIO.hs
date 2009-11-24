-- | A Peakachu backend to write output to the console

module FRP.Peakachu.Backend.StdIO
  ( stdoutB, interactB
  ) where

import FRP.Peakachu.Backend (Backend(..), Sink(..))
import Control.Concurrent.MVar.YC (writeMVar)

import Control.Concurrent.MVar (newMVar, readMVar)
import Control.Monad (when)
import Data.Function (fix)
import Data.Monoid (mempty)
import System.IO (hFlush, stdout)

outSink :: Sink String
outSink =
  mempty { sinkConsume = consume }
  where
    consume x = do
      putStr x
      hFlush stdout

stdoutB :: Backend String ()
stdoutB =
  Backend . return . return $ outSink

interactB :: Backend String String
interactB =
  Backend $ f
  where
    f handler = do
      resumeVar <- newMVar True
      return outSink
        { sinkQuitLoop = writeMVar resumeVar False
        , sinkMainLoop = Just . fix $ \resume -> do
            r <- readMVar resumeVar
            when r $ do
              getLine >>= handler
              resume
        }


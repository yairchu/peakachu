-- | A Peakachu backend to write output to the console

module FRP.Peakachu.Backend.StdIO (stdoutB) where

import FRP.Peakachu.Backend (Backend(..), Sink(..))

import Data.Monoid (mempty)
import System.IO (hFlush, stdout)

stdoutB :: Backend String ()
stdoutB =
  Backend . const . return $ mempty
  { sinkConsume = consume
  }
  where
    consume x = do
      putStr x
      hFlush stdout


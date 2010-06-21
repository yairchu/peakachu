{-# LANGUAGE TemplateHaskell #-}

module FRP.Peakachu.Backend.Internal
    ( Sink(..), MainLoop(..), ParallelIO(..)
    ) where

import Data.Newtype (mkInNewtypeFuncs)

import Control.Concurrent (forkIO)
import Control.Instances () -- IO Monoids
import Data.DeriveTH (derive, makeMonoid)
import Data.Monoid

newtype ParallelIO = ParIO { runParIO :: IO () }
$(mkInNewtypeFuncs [1,2] ''ParallelIO)

instance Monoid ParallelIO where
    mempty = ParIO mempty
    mappend a = inParallelIO2 (>>) (inParallelIO1 ((>> return ()) . forkIO) a)

data MainLoop =
    MainLoop
    { mlInit :: IO ()
    , mlQuit :: IO ()
    , mlRun :: Maybe ParallelIO
    }
$(derive makeMonoid ''MainLoop)

data Sink a = Sink
    { sinkConsume :: a -> IO ()
    , sinkMainLoop :: MainLoop
    }

-- not using "derive" to derive Sink's Monoid because it has a bug
instance Monoid (Sink a) where
    mempty = Sink mempty mempty
    mappend (Sink x0 x1) (Sink y0 y1) =
        Sink (mappend x0 y0) (mappend x1 y1)


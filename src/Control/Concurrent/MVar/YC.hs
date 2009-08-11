module Control.Concurrent.MVar.YC (
  modifyMVarPure, writeMVar
  ) where

import Control.SECombinator (result)

import Control.Concurrent.MVar (MVar, modifyMVar_)

modifyMVarPure :: MVar a -> (a -> a) -> IO ()
modifyMVarPure mvar = modifyMVar_ mvar . result return

writeMVar :: MVar a -> a -> IO ()
writeMVar mvar = modifyMVarPure mvar . const



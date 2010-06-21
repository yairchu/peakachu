module Control.Concurrent.MVar.YC (
    modifyMVarPure, writeMVar
    ) where

import Control.Applicative ()
import Control.Concurrent.MVar (MVar, modifyMVar_)

modifyMVarPure :: MVar a -> (a -> a) -> IO ()
modifyMVarPure mvar = modifyMVar_ mvar . fmap return

writeMVar :: MVar a -> a -> IO ()
writeMVar mvar = modifyMVarPure mvar . const


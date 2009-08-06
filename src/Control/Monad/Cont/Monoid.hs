{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.Cont.Monoid (
  inContT, inContT2
  ) where

import Control.SECombinator (argument, result)

import Control.Monad (liftM2)
import Control.Monad.Cont (ContT(..))
import Control.Monad.Reader ()
import Data.Monoid (Monoid(..))

type InContT r m a = (a -> m r) -> m r

inContT ::
  (InContT r0 m0 a0 -> InContT r1 m1 a1) ->
  ContT r0 m0 a0 -> ContT r1 m1 a1
inContT func = ContT . func . runContT

inContT2 ::
  (InContT r0 m0 a0 -> InContT r1 m1 a1 -> InContT r2 m2 a2) ->
  ContT r0 m0 a0 -> ContT r1 m1 a1 -> ContT r2 m2 a2
inContT2 = result inContT . argument runContT

instance (Monad m, Monoid (m r)) => Monoid (ContT r m a) where
  mempty = ContT (return mempty)
  mappend = inContT2 (liftM2 mappend)


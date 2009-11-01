{-# LANGUAGE TemplateHaskell #-}

module Data.InfiniteStream
  ( InfiniteStreamT(..)
  ) where

import Control.Lift
import Data.InfiniteStream.Item

import Control.Applicative
import Control.Category
import Control.Compose
import Data.Bijection
import Data.Monoid
import Prelude hiding ((.))

newtype InfiniteStreamT f a = InfStrT
  { runInfStrT :: f (InfiniteStreamItem (InfiniteStreamT f) a)
  }

biInfStrT
  :: Bijection (->)
     (f (InfiniteStreamItem (InfiniteStreamT f) a))
     (InfiniteStreamT f a)
biInfStrT = Bi InfStrT runInfStrT

instance Applicative f => MakeApplicative (InfiniteStreamT f) where
  applifter = biLift (biInfStrT . inverse biO) ~. oLift appLift ~. oidLift ~. appLift
$(mkApplicative <$> [d| instance Applicative f => Applicable (InfiniteStreamT f) |])

instance (Applicative f, Monoid a) => Monoid (InfiniteStreamT f a) where
  mempty  = pure mempty
  mappend = liftA2 mappend


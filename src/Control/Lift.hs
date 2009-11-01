{-# LANGUAGE FlexibleInstances, Rank2Types, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Lift 
  ( MakeApplicative(..), Lift(..)
  , uplift0, uplift1, uplift2
  , compLifts, appLift, bijLift
  ) where

import Control.Applicative (Applicative(..), liftA2)
import Control.Compose (Id(..), inId, inId2)
import Data.Bijection (Bijection(..))

data Lift f g = Lift
  { lift0 :: forall a. f a -> g a
  , lift1 :: forall a b. (f a -> f b) -> g a -> g b
  , lift2 :: forall a b c. (f a -> f b -> f c) -> g a -> g b -> g c
  }

uplift0 :: Lift Id f -> a -> f a
uplift0 l = lift0 l . Id

uplift1 :: Lift Id f -> (a -> b) -> f a -> f b
uplift1 l = lift1 l . inId

uplift2 :: Lift Id f -> (a -> b -> c) -> f a -> f b -> f c
uplift2 l = lift2 l . inId2

class MakeApplicative f where
  lifter :: Lift Id f

instance MakeApplicative f => Functor f where
  fmap  = uplift1 lifter
instance MakeApplicative f => Applicative f where
  pure  = uplift0 lifter
  (<*>) = uplift2 lifter ($)

-- Lift is almost a Category
-- except it is of kind :: (* -> *) -> (* -> *) -> *
-- and not :: * -> * -> *
compLifts :: Lift b c -> Lift a b -> Lift a c
compLifts left right =
  Lift
  { lift0 = lift0 left . lift0 right
  , lift1 = lift1 left . lift1 right
  , lift2 = lift2 left . lift2 right
  }

withId :: (Id a -> Id b) -> a -> b
withId = (unId .) . (. Id)

withId2 :: (Id a -> Id b -> Id c) -> a -> b -> c
withId2 f a b = unId $ f (Id a) (Id b)

appLift :: Applicative f => Lift Id f
appLift =
  Lift
  { lift0 = pure . unId
  , lift1 = fmap . withId
  , lift2 = liftA2 . withId2
  }

bijLift :: (forall a. Bijection (->) (f a) (g a)) -> Lift f g
bijLift bij =
  Lift l0 l1 l2
  where
    l0 = biTo bij
    l1 = (biTo bij .) . (. biFrom bij)
    l2 f a b = biTo bij $ f (biFrom bij a) (biFrom bij b)


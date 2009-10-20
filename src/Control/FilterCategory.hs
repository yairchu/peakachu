module Control.FilterCategory where

import Control.Category (Category(..))

import Prelude hiding ((.), id)

class Category cat => FilterCategory cat where
  rid :: cat (Maybe a) a

arr :: (Category cat, Functor (cat a)) => (a -> b) -> cat a b
arr = (`fmap` id)

mapMaybeC
  :: (FilterCategory cat, Functor (cat a))
  => (a -> Maybe b) -> cat a b
mapMaybeC f = rid . arr f


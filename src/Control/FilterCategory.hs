module Control.FilterCategory
  ( FilterCategory(..)
  , rid, mapMaybeC, filterC
  ) where

import Control.Category (Category(..))
import Control.Monad (guard)
import Data.Foldable (Foldable(..), toList)

import Prelude hiding ((.), id)

class Category cat => FilterCategory cat where
  flattenC :: cat [a] a
  arrC :: (a -> b) -> cat a b

rid :: (FilterCategory cat, Foldable f) => cat (f a) a
rid = flattenC . arrC toList

mapMaybeC :: FilterCategory cat => (a -> Maybe b) -> cat a b
mapMaybeC f = rid . arrC f

filterC :: FilterCategory cat => (a -> Bool) -> cat a a
filterC cond =
  mapMaybeC f
  where
    f x = do
      guard $ cond x
      return x


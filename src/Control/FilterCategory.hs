module Control.FilterCategory
  ( FilterCategory(..)
  , rid, mapMaybeC
  ) where

import Control.Category (Category(..))
import Data.Foldable (Foldable(..), toList)

import Prelude hiding ((.), id)

class Category cat => FilterCategory cat where
  flattenC :: cat [a] a
  arrC :: (a -> b) -> cat a b

rid :: (FilterCategory cat, Foldable f)
  => cat (f a) a
rid = flattenC . arrC toList

mapMaybeC :: (FilterCategory cat, Foldable f)
  => (a -> f b) -> cat a b
mapMaybeC f = rid . arrC f


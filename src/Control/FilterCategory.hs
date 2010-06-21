-- | A FilterCategory is a Category that supports mapMaybeC.
--
-- In Peakachu, both Program and Backend are instances of FilterCategory.

module Control.FilterCategory
    ( FilterCategory(..)
    , genericFlattenC, mapMaybeC, filterC
    ) where

import Control.Category (Category(..))
import Control.Monad (guard)
import Data.Foldable (Foldable(..), toList)

import Prelude hiding ((.), id)

class Category cat => FilterCategory cat where
    flattenC :: cat [a] a
    arrC :: (a -> b) -> cat a b

genericFlattenC :: (FilterCategory cat, Foldable f) => cat (f a) a
genericFlattenC = flattenC . arrC toList

mapMaybeC :: FilterCategory cat => (a -> Maybe b) -> cat a b
mapMaybeC f = genericFlattenC . arrC f

filterC :: FilterCategory cat => (a -> Bool) -> cat a a
filterC cond =
    mapMaybeC f
    where
        f x = do
            guard $ cond x
            return x


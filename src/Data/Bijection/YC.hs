-- | In/with functions for bijective functions
--
-- Example:
--
-- > import Data.Bijection (Bijection(..), bimap)
-- > import Data.Bijection.YC (withBi2)
-- > import Data.Monoid (Monoid(..), Sum(..))
-- >
-- > biSum :: Num a => Bijection (->) a (Sum a)
-- > biSum = Bi Sum getSum
-- >
-- > > withBi2 (bimap biSum) mappend (Just 5) (Just 7)
-- > Just 12
-- >
-- > > withBi2 (bimap biSum) mappend Nothing (Just 7)
-- > Just 7

module Data.Bijection.YC
  ( withBi, inBi2, withBi2
  ) where

import Control.Arrow (Arrow)
import Data.Bijection (Bijection(..), inBi, inverse)

withBi :: Arrow x => Bijection x a b -> x b b -> x a a
withBi = inBi . inverse

inBi2 :: Bijection (->) a b -> (a -> a -> a) -> b -> b -> b
inBi2 bi func left right =
  biTo bi $ func (biFrom bi left) (biFrom bi right)

withBi2 :: Bijection (->) a b -> (b -> b -> b) -> a -> a -> a
withBi2 = inBi2 . inverse


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


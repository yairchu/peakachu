{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module FRP.Peakachu.Program
  ( Program(..), scanlP, singleValueP, loopbackP
  ) where

import Control.FilterCategory (FilterCategory(..))

import Control.Applicative (Applicative(..), (<$>))
import Control.Category (Category(..))
import Control.Compose
import Data.DeriveTH
import Data.Function (fix)
import Data.Newtype
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Monoid (Monoid(..))

import Prelude hiding ((.), id)

data InfiStream a = InfiStream
  { headIS :: a
  , tailIS :: InfiStream a
  }
$(derive makeFunctor ''InfiStream)

instance Applicative InfiStream where
  pure x = InfiStream x (pure x)
  InfiStream x xs <*> InfiStream y ys =
    InfiStream (x y) (xs <*> ys)

newtype InfiStrTrans a b = InfiStrTrans
  { runInfiStrTrans :: O InfiStream ((->) a) b
  } deriving (Applicative, Functor)
$(mkNewtypeInFuncs 1 ''InfiStrTrans)

instance Monoid b => Monoid (InfiStrTrans a b) where
  mempty = pure mempty
  mappend (InfiStrTrans left) (InfiStrTrans right) = undefined

blah :: Monoid b => InfiStrTrans a b -> InfiStrTrans a b -> InfiStrTrans a b
blah = mappend

-- | Program is similar to 
-- ListT ((->) input) [output].
-- Differences:
-- * You know the head
--   (similar to ListItem (ListT ((->) input)) [output])
data Program input output = Program
  { progVals :: [output]
  , progMore :: input -> Program input output
  }

instance Category Program where
  id =
    f []
    where
      f = (`Program` g)
      g = f . return
  a . Program valsB restB =
    Program
    { progVals = stuff >>= progVals
    , progMore = more
    }
    where
      stuff = scanl step a valsB
      step (Program _ restA) xB = restA xB
      more x = Program [] (progMore (last stuff)) . restB x

instance FilterCategory Program where
  rid =
    fromJust <$> t []
    where
      t = (`Program` t . filter isJust . return)

instance Functor (Program input) where
  fmap f (Program vals rest) =
    Program (fmap f vals) ((fmap . fmap) f rest)

instance Applicative (Program input) where
  pure x =
    Program (repeat x) ((pure . pure) x)
  Program valsA restA <*> Program valsB restB =
    Program
    { progVals = zipWith ($) valsA valsB
    , progMore = more <$> restA <*> restB
    }
    where
      more a b =
        case (a, b) of
          (Program [] _, Program [] _) -> Program [] rMore
          _ -> r
        where
          r@(Program _ rMore) = p valsA a <*> p valsB b
      p [] (Program [] m) = Program [] m
      p (x:_) (Program [] m) = Program [x] m
      p _ s = s

instance Monoid (Program input output) where
  mempty = Program [] mempty
  mappend (Program vA rA) (Program vB rB) =
    Program (mappend vA vB) (mappend rA rB)

scanlP :: (s -> i -> s) -> s -> Program i s
scanlP step =
  fix $ \self ->
  Program <$> return <*> fmap self . step

singleValueP :: b -> Program a b
singleValueP x = Program [x] mempty

loopbackPh :: Program a (Either b a) -> Program a b
loopbackPh program =
  Program
  { progVals = stuff >>= mapMaybe gLeft . progVals
  , progMore = fmap loopbackPh . progMore . last $ stuff
  }
  where
    gLeft (Left x) = Just x
    gLeft _ = Nothing
    gRight (Right x) = Just x
    gRight _ = Nothing
    stuff =
      scanl step program
      . mapMaybe gRight
      . progVals $ program
    step (Program _ rest) x = rest x

loopbackP :: Program b a -> Program a b -> Program a b
loopbackP loop =
  loopbackPh .
  (.) (mappend (Left <$> id) (Right <$> loop))


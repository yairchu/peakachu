module FRP.Peakachu.Program
  ( Program(..), scanlP, singleValueP, loopbackP
  ) where

import Control.FilterCategory (FilterCategory(..))

import Control.Applicative (Applicative(..), (<$>), liftA2)
import Control.Category (Category(..))
import Data.Function (fix)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Monoid (Monoid(..))

import Prelude hiding ((.), id)

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
    Program (pure x) ((pure . pure) x)
  Program valsA restA <*> Program valsB restB =
    Program
    { progVals = reverse $ reverse valsA <*> reverse valsB
    , progMore = liftA2 more restA restB
    }
    where
      more a b =
        case (a, b) of
          (Program [] _, Program [] _) -> Program [] rMore
          _ -> r
        where
          r@(Program _ rMore) = p valsA a <*> p valsB b
      p [] (Program [] m) = Program [] m
      p prevV (Program [] m) = Program [last prevV] m
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


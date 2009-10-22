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
-- * You know if you have the last item
data Program input output = Program
  { progVals :: [output]
  , progMore :: Maybe (input -> Program input output)
  }

instance Category Program where
  id =
    f []
    where
      f = (`Program` Just g)
      g = f . return
  a . Program valsB restB =
    Program
    { progVals = stuff >>= progVals
    , progMore = fmap more restB
    }
    where
      stuff = scanl step a valsB
      step (Program _ restA) xB = maybe mempty ($ xB) restA
      more rB x = Program [] (progMore (last stuff)) . rB x

instance FilterCategory Program where
  rid =
    fromJust <$> t []
    where
      t = (`Program` Just (t . filter isJust . return))

instance Functor (Program input) where
  fmap f (Program vals rest) =
    Program (fmap f vals) ((fmap . fmap . fmap) f rest)

instance Applicative (Program input) where
  pure x =
    Program (pure x) ((pure . pure . pure) x)
  Program valsA restA <*> Program valsB restB =
    Program
    { progVals = reverse $ zipWith ($) (reverse valsA) (reverse valsB)
    , progMore = (liftA2 . liftA2) more restA restB
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
  mempty = Program [] Nothing
  mappend (Program vA rA) (Program vB rB) =
    Program (mappend vA vB) (mappend rA rB)

scanlP :: (s -> i -> s) -> s -> Program i s
scanlP step =
  fix $ \self ->
  Program <$> return <*> return . fmap self . step

singleValueP :: b -> Program a b
singleValueP x = Program [x] Nothing

loopbackPh :: Program a (Either b a) -> Program a b
loopbackPh program =
  Program
  { progVals = stuff >>= mapMaybe gLeft . progVals
  , progMore = (fmap . fmap) loopbackPh . progMore . last $ stuff
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
    step (Program _ rest) x = maybe mempty ($ x) rest

loopbackP :: Program b a -> Program a b -> Program a b
loopbackP loop =
  loopbackPh .
  (.) (mappend (Left <$> id) (Right <$> loop))


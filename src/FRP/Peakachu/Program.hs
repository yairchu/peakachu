module FRP.Peakachu.Program
  ( Program(..), filterS, mapMaybeS, scanlS
  ) where

import Control.Applicative (Applicative(..), (<$>), liftA2)
import Control.Category (Category(..))
import Data.Maybe (fromJust, isJust)
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
      step (Program _ Nothing) _ = Program [] Nothing
      step (Program _ (Just restA)) xB = restA xB
      more rB x = Program [] (progMore (last stuff)) . rB x

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

filterS :: (a -> Bool) -> Program a a
filterS f =
  t []
  where
    t = (`Program` Just (t . filter f . return))

mapMaybeS :: (a -> Maybe b) -> Program a b
mapMaybeS f = fromJust <$> filterS isJust . (f <$> id)

scanlS :: (s -> i -> s) -> s -> Program i s
scanlS step =
  liftA2 Program return (return . fmap (scanlS step) . step)


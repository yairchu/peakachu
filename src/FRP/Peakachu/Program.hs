{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module FRP.Peakachu.Program
  ( Program(..), MergeProgram(..), AppendProgram(..)
  , ProgCat(..)
  , singleValueP, lstP, lstPs, delayP
  ) where

import Control.FilterCategory (FilterCategory(..), genericFlattenC)
import Data.ADT.Getters (mkADTGetters)
import Data.Bijection.YC (withBi2)
import Data.Newtype (mkWithNewtypeFuncs)

import Control.Applicative (Applicative(..), (<$>), liftA2)
import Control.Category (Category(..))
import Control.Monad (MonadPlus(..), ap)
import Data.Bijection (Bijection(..), bimap)
import Data.Generics.Aliases (orElse)
import Data.List (genericDrop, genericTake)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Monoid (Monoid(..))

import Prelude hiding ((.), id)

data Program a b = Program
  { progVals :: [b]
  , progMore :: Maybe (a -> Program a b)
  }

class FilterCategory prog => ProgCat prog where
  scanlP :: (b -> a -> b) -> b -> prog a b
  emptyP :: prog a b
  takeWhileP :: (a -> Bool) -> prog a a
  loopbackP :: prog a (Either a b) -> prog a b

singleValueP :: ProgCat prog => prog a ()
singleValueP = scanlP const () . emptyP

delayP :: (Integral i, ProgCat prog) => i -> prog a a
delayP n =
  flattenC . arrC (genericDrop n) . scanlP step []
  where
    step xs = (: genericTake n xs)

instance Category Program where
  id =
    Program [] (Just f)
    where
      f x = Program [x] (Just f)
  left . right =
    Program (catMaybes stuff >>= progVals) more
    where
      Program rightStart rightMore = right
      stuff = scanl step (Just left) rightStart
      step l valRight = do
        Program _ moreLeft <- l
        moreFunc <- moreLeft
        return $ moreFunc valRight
      more = do
        moreFunc <- rightMore
        lastStuff <- last stuff
        return $ (.) (Program [] (progMore lastStuff)) . moreFunc

instance Functor (Program a) where
  fmap f p =
    Program
    { progVals = fmap f . progVals $ p
    , progMore = (fmap . fmap . fmap) f . progMore $ p
    }

instance FilterCategory Program where
  flattenC =
    f []
    where
      f = (`Program` Just f)
  arrC = (<$> id)

$(mkADTGetters ''Either)

instance ProgCat Program where
  emptyP = Program [] Nothing
  scanlP step start =
    Program [start] $ Just (scanlP step . step start)
  takeWhileP cond =
    Program [] (Just f)
    where
      f x
        | cond x = Program [x] (Just f)
        | otherwise = Program [] Nothing
  loopbackP program =
    Program
    { progVals = stuff >>= mapMaybe gRight . progVals
    , progMore = (fmap . fmap) loopbackP . progMore . last $ stuff
    }
    where
      stuff =
        scanl step program
        . mapMaybe gLeft . progVals $ program
      step prev val =
        maybe emptyP ($ val) (progMore prev)

newtype MergeProgram a b = MergeProg
  { runMergeProg :: Program a b
  } deriving (Category, FilterCategory, Functor, ProgCat)

$(mkWithNewtypeFuncs [2] ''MergeProgram)

biMergeProg :: Bijection (->) (Program a b) (MergeProgram a b)
biMergeProg = Bi MergeProg runMergeProg

instance Monoid (MergeProgram a b) where
  mempty = emptyP
  mappend (MergeProg left) (MergeProg right) =
    MergeProg Program
    { progVals =
      mappend (progVals left) (progVals right)
    , progMore =
      withBi2 ((bimap . bimap) biMergeProg)
      mappend (progMore left) (progMore right)
    }

instance Applicative (MergeProgram a) where
  pure x =
    MergeProg Program
    { progVals = pure x
    , progMore = pure . pure . runMergeProg . pure $ x
    }
  MergeProg left <*> MergeProg right =
    MergeProg Program
    { progVals = progVals left <*> progVals right
    , progMore =
      (liftA2 . liftA2 . withMergeProgram2)
      (<*>) (progMore left) (progMore right)
    }

newtype AppendProgram a b = AppendProg
  { runAppendProg :: Program a b
  } deriving (Category, FilterCategory, Functor, ProgCat)

$(mkWithNewtypeFuncs [1,2] ''AppendProgram)

instance Monoid (AppendProgram a b) where
  mempty = emptyP
  mappend (AppendProg left) (AppendProg right) =
    AppendProg $
    case progMore left of
      Nothing -> Program
        { progVals = progVals left ++ progVals right
        , progMore = progMore right
        }
      Just more -> Program
        { progVals = progVals left
        , progMore = Just $ flip (withAppendProgram2 mappend) right <$> more
        }

instance Monad (AppendProgram a) where
  return x = AppendProg $ Program [x] Nothing
  AppendProg left >>= right =
    mconcat $ map right (progVals left) ++ [rest]
    where
      rest =
        AppendProg Program
        { progVals = []
        , progMore =
          (fmap . fmap . withAppendProgram1)
          (>>= right) (progMore left)
        }

instance MonadPlus (AppendProgram a) where
  mzero = mempty
  mplus = mappend

instance Applicative (AppendProgram a) where
  pure = return
  (<*>) = ap

lstPs :: ProgCat prog => Maybe b -> (a -> Maybe b) -> prog a b
lstPs start f =
  genericFlattenC . scanlP (flip orElse) start . arrC f

lstP :: ProgCat prog => (a -> Maybe b) -> prog a b
lstP = lstPs Nothing


{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module FRP.Peakachu.Program
  ( Program(..), MergeProgram(..), AppendProgram(..)
  , ProgCat(..)
  , singleValueP
  , loopbackP, lstP, lstPs
  , inMergeProgram1
  ) where

import Control.FilterCategory (FilterCategory(..), rid)
import Data.Newtype

import Control.Applicative (Applicative(..), (<$>), liftA2)
import Control.Category (Category(..))
import Control.Monad (MonadPlus(..), ap)
import Data.ADT.Getters (mkADTGetters)
import Data.Generics.Aliases (orElse)
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

singleValueP :: ProgCat prog => prog a ()
singleValueP = scanlP const () . emptyP

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

newtype MergeProgram a b = MergeProg
  { runMergeProg :: Program a b
  } deriving (Category, FilterCategory, Functor, ProgCat)

$(mkInNewtypeFuncs [1] ''MergeProgram)
$(mkWithNewtypeFuncs [2] ''MergeProgram)

instance Monoid (MergeProgram a b) where
  mempty = emptyP
  mappend (MergeProg left) (MergeProg right) =
    MergeProg Program
    { progVals = progVals left ++ progVals right
    , progMore =
      (fmap . fmap) runMergeProg $
      mappend
      ((fmap . fmap) MergeProg (progMore left))
      ((fmap . fmap) MergeProg (progMore right))
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
          (fmap . fmap . withAppendProgram1) (>>= right)
          . progMore $ left
        }

instance MonadPlus (AppendProgram a) where
  mzero = mempty
  mplus = mappend

instance Applicative (AppendProgram a) where
  pure = return
  (<*>) = ap

$(mkADTGetters ''Either)

loopbackPh :: Program a (Either b a) -> Program a b
loopbackPh program =
  Program
  { progVals = stuff >>= mapMaybe gLeft . progVals
  , progMore = (fmap . fmap) loopbackPh . progMore . last $ stuff
  }
  where
    stuff =
      scanl step program
      . mapMaybe gRight . progVals $ program
    step prev val =
      maybe emptyP ($ val) (progMore prev)

loopbackP :: Program b a -> Program a b -> Program a b
loopbackP loop =
  loopbackPh . (.)
  (withMergeProgram2 mappend (Left <$> id) (Right <$> loop))

lstPs :: ProgCat prog => (Maybe b) -> (a -> Maybe b) -> prog a b
lstPs start f =
  rid . scanlP (flip orElse) start . arrC f

lstP :: ProgCat prog => (a -> Maybe b) -> prog a b
lstP = lstPs Nothing


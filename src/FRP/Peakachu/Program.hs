{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module FRP.Peakachu.Program
  ( Program(..), MergeProgram(..), AppendProgram(..)
  , scanlP, loopbackP, lstP, lstPs
  , inMergeProgram1
  ) where

import Control.FilterCategory (FilterCategory(..), arr)
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
  rid =
    f Nothing
    where
      f Nothing = Program [] (Just f)
      f (Just x) = Program [x] (Just f)

emptyProgram :: Program a b
emptyProgram = Program [] Nothing

newtype MergeProgram a b = MergeProg
  { runMergeProg :: Program a b
  } deriving (Category, FilterCategory, Functor)

$(mkInNewtypeFuncs [1] ''MergeProgram)
$(mkWithNewtypeFuncs [2] ''MergeProgram)

instance Monoid (MergeProgram a b) where
  mempty = MergeProg emptyProgram
  mappend (MergeProg left) (MergeProg right) =
    MergeProg $ Program
    { progVals = progVals left ++ progVals right
    , progMore =
      (fmap . fmap) runMergeProg $
      mappend
      ((fmap . fmap) MergeProg (progMore left))
      ((fmap . fmap) MergeProg (progMore right))
    }

instance Applicative (MergeProgram a) where
  pure x =
    MergeProg $ Program
    { progVals = repeat x
    , progMore = pure . pure . runMergeProg . pure $ x
    }
  MergeProg left <*> MergeProg right =
    MergeProg $ Program
    { progVals = zipWith ($) (progVals left) (progVals right)
    , progMore =
      (liftA2 . liftA2 . withMergeProgram2)
      (<*>) (progMore left) (progMore right)
    }

newtype AppendProgram a b = AppendProg
  { runAppendProg :: Program a b
  } deriving (Category, FilterCategory, Functor)

$(mkWithNewtypeFuncs [1,2] ''AppendProgram)

instance Monoid (AppendProgram a b) where
  mempty = AppendProg emptyProgram
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
        AppendProg $ Program
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

scanlP :: (s -> i -> s) -> s -> Program i s
scanlP step start =
  Program [start] $ Just (scanlP step . step start)

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
      maybe emptyProgram ($ val) (progMore prev)

loopbackP :: Program b a -> Program a b -> Program a b
loopbackP loop =
  loopbackPh . (.)
  (withMergeProgram2 mappend (Left <$> id) (Right <$> loop))

lstPs :: (Maybe b) -> (a -> Maybe b) -> MergeProgram a b
lstPs start f =
  rid . MergeProg (scanlP (flip orElse) start) . arr f

lstP :: (a -> Maybe b) -> MergeProgram a b
lstP = lstPs Nothing


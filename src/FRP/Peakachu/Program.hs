{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module FRP.Peakachu.Program
  ( Program(..), scanlP, singleValueP, loopbackP
  , mergeFinProgs
  ) where

import Control.FilterCategory (FilterCategory(..))

import Control.Applicative (Applicative(..), (<$>), ZipList(..), liftA2)
import Control.Applicative.Define
import Control.Category (Category(..))
import Control.Compose
import Control.Monad (MonadPlus(..), liftM, ap)
import Data.Function (fix)
import Data.Maybe (fromJust, isJust, mapMaybe, catMaybes)
import Data.Monoid (Monoid(..))
import Data.Newtype

import Prelude hiding ((.), id)

data InfiniteStreamItem m a = InfStrIt
  { headIS :: a
  , tailIS :: m a
  }
$(mkWithNewtypeFuncs [0..2] ''Id)
$(mkApplicative
  [d| instance Applicable f => Applicable (InfiniteStreamItem f) |]
  [["withId", "lift"], ["lift"]])
instance (Applicative f, Monoid a) => Monoid (InfiniteStreamItem f a) where
  mempty  = pure mempty
  mappend = liftA2 mappend

newtype InfiniteStreamT f a = InfStrT
  { runInfStrT :: f (InfiniteStreamItem (InfiniteStreamT f) a)
  }
$(mkApplicative [d| instance Applicable f => Applicable (InfiniteStreamT f) |]
  [["lift", "lift"]])
instance (Applicative f, Monoid a) => Monoid (InfiniteStreamT f a) where
  mempty  = pure mempty
  mappend = liftA2 mappend

newtype InfiniteProgram a b = InfProg
  { runInfProg :: InfiniteStreamItem (InfiniteStreamT ((->) a)) [b]
  } deriving Monoid
$(mkWithNewtypeFuncs [2] ''InfiniteProgram)
$(mkWithNewtypeFuncs [0..2] ''ZipList)
$(mkApplicative [d| instance Applicable (InfiniteProgram a) |]
  [["lift", "withZipList", "lift"]])

newtype FiniteProgram a b = FinProg
  { runFinProg :: InfiniteStreamItem (InfiniteStreamT (O Maybe ((->) a))) [b]
  }
$(mkInNewtypeFuncs [2] ''FiniteProgram)
$(mkWithNewtypeFuncs [1,2] ''FiniteProgram)

instance Category InfiniteProgram where
  id =
    InfProg . InfStrIt [] $ f
    where
      f = InfStrT $ (`InfStrIt` f) . return
  left . right =
    InfProg . InfStrIt (stuff >>= headIS) . InfStrT $ more
    where
      InfStrIt rightStart rightMore = runInfProg right
      stuff = scanl (runInfStrT . tailIS) (runInfProg left) rightStart
      more =
        withInfiniteProgram2 (.)
        (InfStrIt [] (tailIS (last stuff)))
        . runInfStrT rightMore

instance Category FiniteProgram where
  id =
    FinProg . InfStrIt [] $ f
    where
      f = InfStrT . O . Just $ (`InfStrIt` f) . return
  left . right =
    FinProg . InfStrIt (catMaybes stuff >>= headIS) . InfStrT . O $ more
    where
      InfStrIt rightStart rightMore = runFinProg right
      stuff = scanl step (Just (runFinProg left)) rightStart
      step l valRight = do
        InfStrIt _ moreLeft <- l
        moreFunc <- unO . runInfStrT $ moreLeft
        return $ moreFunc valRight
      more = do
        moreFunc <- unO . runInfStrT $ rightMore
        lastStuff <- last stuff
        return $
          withFiniteProgram2 (.)
          (InfStrIt [] (tailIS lastStuff))
          . moreFunc

appendPrograms :: FiniteProgram a b -> FiniteProgram a b -> FiniteProgram a b
appendPrograms left right =
  FinProg $ case unO (runInfStrT leftMore) of
    Nothing -> InfStrIt (leftStart ++ rightStart) rightMore
    Just more ->
      InfStrIt leftStart
      . InfStrT . O . Just
      $ withFiniteProgram1 (`appendPrograms` right) . more
  where
    InfStrIt leftStart leftMore = runFinProg left
    InfStrIt rightStart rightMore = runFinProg right

doneProgram :: FiniteProgram a b
doneProgram = FinProg . InfStrIt [] . InfStrT . O $ Nothing

concatPrograms :: [FiniteProgram a b] -> FiniteProgram a b
concatPrograms = foldr appendPrograms doneProgram

mergeFinProgs :: FiniteProgram a b -> FiniteProgram a b -> FiniteProgram a b
mergeFinProgs = inFiniteProgram2 mappend

instance Monad (FiniteProgram a) where
  return = FinProg . (`InfStrIt` InfStrT (O Nothing)) . return
  left >>= right =
    concatPrograms
    . (map right leftStart ++)
    . return
    . FinProg
    . InfStrIt []
    . InfStrT
    . O
    . fmap (withFiniteProgram1 (>>= right) .)
    . unO
    . runInfStrT
    $ leftMore
    where
      InfStrIt leftStart leftMore = runFinProg left

instance Functor (FiniteProgram a) where
  fmap  = liftM
instance Applicative (FiniteProgram a) where
  pure  = return
  (<*>) = ap

instance MonadPlus (FiniteProgram a) where
  mzero = doneProgram
  mplus = appendPrograms

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
      stuff = scanl progMore a valsB
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
      scanl progMore program
      . mapMaybe gRight
      . progVals $ program

loopbackP :: Program b a -> Program a b -> Program a b
loopbackP loop =
  loopbackPh .
  (.) (mappend (Left <$> id) (Right <$> loop))


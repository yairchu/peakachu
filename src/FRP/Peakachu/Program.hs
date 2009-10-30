{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module FRP.Peakachu.Program
  ( Program(..), scanlP, singleValueP, loopbackP
  ) where

import Control.FilterCategory (FilterCategory(..))

import Control.Applicative (Applicative(..), (<$>), ZipList(..), liftA2)
import Control.Applicative.Define
import Control.Category (Category(..))
import Control.Compose
import Data.Function (fix)
import Data.Newtype
import Data.Maybe (fromJust, isJust, mapMaybe, catMaybes)
import Data.Monoid (Monoid(..))

import Prelude hiding ((.), id)

-- mtl's Identity is not an Applicative
newtype Identity a = Identity a
$(mkInNewtypeFuncs [0..2] ''Identity)
$(mkWithNewtypeFuncs [0..2] ''Identity)

data InfiniteStreamItem m a = InfStrIt
  { headIS :: a
  , tailIS :: m a
  }

newtype InfiniteStreamT f a = InfStrT
  { runInfStrT :: f (InfiniteStreamItem (InfiniteStreamT f) a)
  }
$(mkInNewtypeFuncs [0..2] ''InfiniteStreamT)

newtype InfiniteProgram a b = InfProg
  { runInfProg :: InfiniteStreamItem (InfiniteStreamT ((->) a)) [b]
  }
$(mkInNewtypeFuncs [0..2] ''InfiniteProgram)
$(mkWithNewtypeFuncs [2] ''InfiniteProgram)
$(mkWithNewtypeFuncs [0..2] ''ZipList)

newtype FiniteProgram a b = FinProg
  { runFinProg :: InfiniteStreamItem (InfiniteStreamT (O Maybe ((->) a))) [b]
  }
$(mkInNewtypeFuncs [1] ''FiniteProgram)
$(mkWithNewtypeFuncs [2] ''FiniteProgram)

lift0 :: Applicative f => a -> f a
lift0 = pure

lift1 :: Functor f => (a -> b) -> f a -> f b
lift1 = fmap

lift2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
lift2 = liftA2

instance Functor f => Functor (InfiniteStreamT f) where
  fmap  = inInfiniteStreamT1 . lift1 . lift1
instance Applicative f => Applicative (InfiniteStreamT f) where
  pure  = inInfiniteStreamT0 . lift0 . lift0
  (<*>) = inInfiniteStreamT2 . lift2 . lift2 $ ($)

$(mkApplicative ''Identity [["inIdentity"]])
$(mkApplicative ''InfiniteProgram [["inInfiniteProgram", "lift", "withZipList", "lift"]])

instance Functor f => Functor (InfiniteStreamItem f) where
  fmap func (InfStrIt x xs) =
    InfStrIt
    ((withIdentity1 . lift1) func x)
    (lift1 func xs)
instance Applicative f => Applicative (InfiniteStreamItem f) where
  pure x =
    InfStrIt
    ((withIdentity0 . lift0) x)
    (lift0 x)
  InfStrIt x xs <*> InfStrIt y ys =
    InfStrIt
    ((withIdentity2 . lift2) ($) x y)
    (lift2 ($) xs ys)

instance Monoid a => Monoid (Identity a) where
  mempty  = lift0 mempty
  mappend = lift2 mappend
instance (Applicative f, Monoid a) => Monoid (InfiniteStreamT f a) where
  mempty  = lift0 mempty
  mappend = lift2 mappend
instance (Applicative f, Monoid a) => Monoid (InfiniteStreamItem f a) where
  mempty  = lift0 mempty
  mappend = lift2 mappend
-- Monoid instance of program:
-- * Can be derived with GeneralizedNewTypeDeriving
-- * Follows a different lifting path than its Applicative
--   (doesn't stem from being an applicative)
instance Monoid (InfiniteProgram a b) where
  mempty  = inInfiniteProgram0 mempty
  mappend = inInfiniteProgram2 mappend

instance Category InfiniteProgram where
  id =
    InfProg . InfStrIt [] $ f
    where
      f = InfStrT $ (`InfStrIt` f) . return
  left . right =
    InfProg . InfStrIt (stuff >>= headIS) . InfStrT $ more
    where
      InfStrIt rightStart rightMore = runInfProg right
      stuff = scanl step (runInfProg left) rightStart
      step (InfStrIt _ moreLeft) valRight = runInfStrT moreLeft valRight
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

instance Monad (FiniteProgram a) where
  return = FinProg . (`InfStrIt` InfStrT (O Nothing)) . return
  left >>= right =
    undefined
    where
      InfStrIt leftStart leftMore = runFinProg left

--data FiniteProgram a b = FinProg
--  { fpVals :: [b]
--  , fpMore :: Maybe (input -> FiniteProgram a b)
--  }

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


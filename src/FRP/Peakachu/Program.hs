{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module FRP.Peakachu.Program
  ( Program(..), MergeProgram(..), AppendProgram(..)
  , scanlP, loopbackP
  ) where

import Control.FilterCategory (FilterCategory(..))
import Control.Lift
import Data.Cons
import Data.InfiniteStream
import Data.Newtype

import Control.Applicative (Applicative(..), (<$>), ZipList(..))
import Control.Category (Category(..))
import Control.Compose
import Control.Monad (MonadPlus(..))
import Data.ADT.Getters
import Data.Bijection
import Data.Maybe (mapMaybe, catMaybes)
import Data.Monoid (Monoid(..))

import Prelude hiding ((.), id)

newtype Program a b = Prog
  { runProg :: Cons (InfiniteStreamT (O Maybe ((->) a))) [b]
  }

biProgram
  :: Bijection (->)
     (Cons (InfiniteStreamT (O Maybe ((->) a))) [b])
     (Program a b)
biProgram = Bi Prog runProg

$(mkInNewtypeFuncs [1,2] ''Program)
$(mkWithNewtypeFuncs [1,2] ''Program)

instance Category Program where
  id =
    Prog . Cons [] $ f
    where
      f = InfStrT . O . Just $ (`Cons` f) . return
  left . right =
    Prog . Cons (catMaybes stuff >>= headC) . InfStrT . O $ more
    where
      Cons rightStart rightMore = runProg right
      stuff = scanl step (Just (runProg left)) rightStart
      step l valRight = do
        Cons _ moreLeft <- l
        moreFunc <- unO . runInfStrT $ moreLeft
        return $ moreFunc valRight
      more = do
        moreFunc <- unO . runInfStrT $ rightMore
        lastStuff <- last stuff
        return $
          withProgram2 (.)
          (Cons [] (tailC lastStuff))
          . moreFunc

instance Functor (Program a) where
  fmap = inProgram1 . fmap . fmap

instance FilterCategory Program where
  rid =
    Prog . Cons [] $ f
    where
      f = InfStrT . O . Just $ g
      g Nothing = Cons [] f
      g (Just x) = Cons [x] f

emptyProgram :: Program a b
emptyProgram = Prog . Cons [] . InfStrT . O $ Nothing

newtype MergeProgram a b = MergeProg
  { runMergeProg :: Program a b
  } deriving (Category, FilterCategory)

biMergeProgram :: Bijection (->) (Program a b) (MergeProgram a b)
biMergeProgram = Bi MergeProg runMergeProg

$(mkInNewtypeFuncs [2] ''MergeProgram)
$(mkWithNewtypeFuncs [2] ''MergeProgram)

instance Monoid (MergeProgram a b) where
  mempty = MergeProg emptyProgram
  mappend = inMergeProgram2 . inProgram2 $ mappend

biZipList :: Bijection (->) [a] (ZipList a)
biZipList = Bi ZipList getZipList

instance MakeApplicative (MergeProgram a) where
  applifter =
    biLift (biMergeProgram . biProgram . inverse biO)
    ~. lifto appLift
    ~. biLift (inverse biZipList)
    ~. appLift
$(mkApplicative <$> [d| instance Applicable (MergeProgram a) |])

newtype AppendProgram a b = AppendProg
  { runAppendProg :: Program a b
  } deriving (Category, FilterCategory)

$(mkWithNewtypeFuncs [1] ''AppendProgram)

instance Monoid (AppendProgram a b) where
  mempty = AppendProg emptyProgram
  mappend left right =
    AppendProg . Prog
    $ case unO (runInfStrT leftMore) of
      Nothing -> Cons (leftStart ++ rightStart) rightMore
      Just more ->
        Cons leftStart
        . InfStrT . O . Just
        $ (withProgram1 . withAppendProgram1) (`mappend` right) . more
    where
      Cons leftStart leftMore   = runProg . runAppendProg $ left
      Cons rightStart rightMore = runProg . runAppendProg $ right

instance Monad (AppendProgram a) where
  return = AppendProg . Prog . (`Cons` InfStrT (O Nothing)) . (:[])
  left >>= right =
    mconcat
    . (map right leftStart ++)
    . (:[])
    . AppendProg
    . Prog
    . Cons []
    . InfStrT
    . O
    . fmap ((withProgram1 . withAppendProgram1) (>>= right) .)
    . unO
    . runInfStrT
    $ leftMore
    where
      Cons leftStart leftMore = runProg . runAppendProg $ left

instance MonadPlus (AppendProgram a) where
  mzero = mempty
  mplus = mappend

$(monadToApplicative <$> [d| instance Applicable (AppendProgram a) |])

scanlP :: (s -> i -> s) -> s -> Program i s
scanlP step start =
  Prog . Cons [start]
  . InfStrT . O . Just
  $ runProg . scanlP step . step start

$(mkADTGetterFuncs ''Either)

$(mkInNewtypeFuncs [1] ''InfiniteStreamT)

loopbackPh :: Program a (Either b a) -> Program a b
loopbackPh program =
  Prog
  . Cons (stuff >>= mapMaybe gLeft . headC)
  . ( ( inInfiniteStreamT1 . inO
      . fmap . fmap . withProgram1)
      loopbackPh
    )
  . tailC . last $ stuff
  where
    rp = runProg
    stuff =
      scanl step (rp program)
      . mapMaybe gRight . headC . rp $ program
    step prev val =
      case (unO . runInfStrT . tailC) prev of
        Nothing -> runProg emptyProgram
        Just more -> more val

loopbackP :: Program b a -> Program a b -> Program a b
loopbackP loop =
  loopbackPh . (.) (withMergeProgram2 mappend (Left <$> id) (Right <$> loop))


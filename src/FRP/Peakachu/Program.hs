{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module FRP.Peakachu.Program
  ( Program(..), MergeProgram(..), AppendProgram(..)
  , scanlP, loopbackP
  ) where

import Control.FilterCategory (FilterCategory(..))
import Control.Lift
import Data.InfiniteStream
import Data.InfiniteStream.Item
import Data.Newtype

import Control.Applicative (Applicative(..), (<$>), ZipList(..))
import Control.Category (Category(..))
import Control.Compose
import Control.Monad (MonadPlus(..))
import Data.ADT.Getters
import Data.Bijection
import Data.Function (fix)
import Data.Maybe (fromJust, isJust, mapMaybe, catMaybes)
import Data.Monoid (Monoid(..))

import Prelude hiding ((.), id)

newtype Program a b = Prog
  { runProg :: InfiniteStreamItem (InfiniteStreamT (O Maybe ((->) a))) [b]
  }

biProgram
  :: Bijection (->)
     (InfiniteStreamItem (InfiniteStreamT (O Maybe ((->) a))) [b])
     (Program a b)
biProgram = Bi Prog runProg

$(mkInNewtypeFuncs [2] ''Program)
$(mkWithNewtypeFuncs [1,2] ''Program)

instance Category Program where
  id =
    Prog . InfStrIt [] $ f
    where
      f = InfStrT . O . Just $ (`InfStrIt` f) . return
  left . right =
    Prog . InfStrIt (catMaybes stuff >>= headIS) . InfStrT . O $ more
    where
      InfStrIt rightStart rightMore = runProg right
      stuff = scanl step (Just (runProg left)) rightStart
      step l valRight = do
        InfStrIt _ moreLeft <- l
        moreFunc <- unO . runInfStrT $ moreLeft
        return $ moreFunc valRight
      more = do
        moreFunc <- unO . runInfStrT $ rightMore
        lastStuff <- last stuff
        return $
          withProgram2 (.)
          (InfStrIt [] (tailIS lastStuff))
          . moreFunc

emptyProgram :: Program a b
emptyProgram = Prog . InfStrIt [] . InfStrT . O $ Nothing

newtype MergeProgram a b = MergeProg
  { runMergeProg :: Program a b
  } deriving Category

biMergeProgram :: Bijection (->) (Program a b) (MergeProgram a b)
biMergeProgram = Bi MergeProg runMergeProg

$(mkInNewtypeFuncs [2] ''MergeProgram)
$(mkWithNewtypeFuncs [1] ''MergeProgram)

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
  } deriving Category

$(mkWithNewtypeFuncs [1] ''AppendProgram)

instance Monoid (AppendProgram a b) where
  mempty = AppendProg emptyProgram
  mappend left right =
    AppendProg . Prog
    $ case unO (runInfStrT leftMore) of
      Nothing -> InfStrIt (leftStart ++ rightStart) rightMore
      Just more ->
        InfStrIt leftStart
        . InfStrT . O . Just
        $ (withProgram1 . withAppendProgram1) (`mappend` right) . more
    where
      InfStrIt leftStart leftMore   = runProg . runAppendProg $ left
      InfStrIt rightStart rightMore = runProg . runAppendProg $ right

instance Monad (AppendProgram a) where
  return = AppendProg . Prog . (`InfStrIt` InfStrT (O Nothing)) . (:[])
  left >>= right =
    mconcat
    . (map right leftStart ++)
    . (:[])
    . AppendProg
    . Prog
    . InfStrIt []
    . InfStrT
    . O
    . fmap ((withProgram1 . withAppendProgram1) (>>= right) .)
    . unO
    . runInfStrT
    $ leftMore
    where
      InfStrIt leftStart leftMore = runProg . runAppendProg $ left

instance MonadPlus (AppendProgram a) where
  mzero = mempty
  mplus = mappend

$(monadToApplicative <$> [d| instance Applicable (AppendProgram a) |])

scanlP :: (s -> i -> s) -> s -> Program i s
scanlP step start =
  Prog . InfStrIt [start]
  . InfStrT . O . Just
  $ runProg . scanlP step . step start

$(mkADTGetterFuncs ''Either)

$(mkInNewtypeFuncs [1] ''InfiniteStreamT)

loopbackPh :: MergeProgram a (Either b a) -> MergeProgram a b
loopbackPh program =
  MergeProg
  . Prog
  . InfStrIt (stuff >>= mapMaybe gLeft . headIS)
  . ( ( inInfiniteStreamT1 . inO
      . fmap . fmap . withProgram1
      . withMergeProgram1) loopbackPh
    )
  . tailIS . last $ stuff
  where
    rp = runProg . runMergeProg
    stuff =
      scanl step (rp program)
      . mapMaybe gRight . headIS . rp $ program
    step prev val =
      case (unO . runInfStrT . tailIS) prev of
        Nothing -> runProg emptyProgram
        Just more -> more val

loopbackP :: MergeProgram b a -> MergeProgram a b -> MergeProgram a b
loopbackP loop =
  loopbackPh . (.) (mappend (Left <$> id) (Right <$> loop))


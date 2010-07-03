{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

-- | @Program a b@ is a pure representation of a computer program,
-- which accepts inputs of type @a@, and outputs values of type @b@.
-- It may also terminate. It can output zero or more @b@ values after each @a@ input.
--
-- * A simple stateless input-output-loop can be created from a function
--   with 'arrC'.
--
-- * A simple stateful input-output-loop can be created using 'scanlP'.
--
-- * Outputs can be filtered using 'filterC'.
--
-- Programs may also be composed together in several ways using common type-classes
--
-- * 'Category': @Program a b -> Program b c -> Program a c@. One program's outputs are fed
--   to another program as input.
--
-- * 'Monoid': @Program a b -> Program a b -> Program a b@. Both programs run in parallel processing the same input. Resulting Program outputs both's outputs.
--
-- * 'Applicative': @Program a (b -> c) -> Program a b -> Program a c@.
--
-- * Alternative `MonadPlus`: 'AppendProgram' is a newtype wrapper whose `Monoid` instance runs one program after the other finishes (like `ZipList` offers an alternative `Applicative` instance for lists). It's also a `Monad` ant its monadic bind allows us to invoke inner programs based on an outer program's outputs.

module FRP.Peakachu.Program
    ( Program(..), AppendProgram(..)
    , scanlP, emptyP, takeWhileP, loopbackP, singleValueP, lstP, lstPs, delayP
    , withAppendProgram1, withAppendProgram2
    ) where

import Control.FilterCategory (FilterCategory(..), genericFlattenC)
import Data.ADT.Getters (mkADTGetters)
import Data.Newtype (mkWithNewtypeFuncs)

import Control.Applicative (Applicative(..), (<$>), liftA2)
import Control.Category (Category(..))
import Control.Monad (MonadPlus(..), ap)
import Data.DeriveTH (derive, makeFunctor)
import Data.List (genericDrop, genericTake)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Monoid (Monoid(..))

import Prelude hiding ((.), id)

-- | A computer program
data Program a b = Program
    { progVals :: [b]
    , progMore :: Maybe (a -> Program a b)
    }
$(derive makeFunctor ''Program)

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

instance FilterCategory Program where
    flattenC =
        f []
        where
            f = (`Program` Just f)
    arrC = (<$> id)

$(mkADTGetters ''Either)

-- | Create a stateful input-output-loop from a simple function
scanlP :: (b -> a -> b) -> b -> Program a b
scanlP step start = Program [start] $ Just (scanlP step . step start)

-- | A program that terminates immediately
emptyP :: Program a b
emptyP = Program [] Nothing

-- | Terminate when a predicate on input fails
takeWhileP :: (a -> Bool) -> Program a a
takeWhileP cond =
    Program [] (Just f)
    where
        f x
            | cond x = Program [x] (Just f)
            | otherwise = Program [] Nothing

-- | Feed some outputs of a 'Program' to itself
loopbackP :: Program a (Either a b) -> Program a b
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

-- | A program that outputs a value and immediately terminates
singleValueP :: Program a ()
singleValueP = scanlP const () . emptyP

-- | Delay the outputs of a 'Program'
delayP :: Integral i => i -> Program a a
delayP n =
    flattenC . arrC (genericDrop n) . scanlP step []
    where
        step xs = (: genericTake n xs)

-- would be nice to derive this.
-- but "derive" currently can't: http://code.google.com/p/ndmitchell/issues/detail?id=270&q=proj:Derive
instance Monoid (Program a b) where
    mempty = Program mempty mempty
    mappend left right =
        Program
        { progVals = mappend (progVals left) (progVals right)
        , progMore = mappend (progMore left) (progMore right)
        }

instance Applicative (Program a) where
    pure x =
        Program
        { progVals = pure x
        , progMore = (pure . pure) (pure x)
        }
    left <*> right =
        Program
        { progVals = progVals left <*> progVals right
        , progMore = (liftA2 . liftA2) (<*>) (progMore left) (progMore right)
        }

-- Combine programs to run in sequence
newtype AppendProgram a b = AppendProg
    { runAppendProg :: Program a b
    } deriving (Category, FilterCategory, Functor)

$(mkWithNewtypeFuncs [1,2] ''AppendProgram)

instance Monoid (AppendProgram a b) where
    mempty = AppendProg emptyP
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

-- | Given a partial function @(a -> Maybe b)@ and a start value, output its most recent result on an input.
lstPs :: Maybe b -> (a -> Maybe b) -> Program a b
lstPs start f =
    genericFlattenC . scanlP (flip mplus) start . arrC f

-- | Given a partial function @(a -> Maybe b)@, output its most recent result on an input.
lstP :: (a -> Maybe b) -> Program a b
lstP = lstPs Nothing


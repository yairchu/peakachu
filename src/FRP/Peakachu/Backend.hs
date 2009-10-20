module FRP.Peakachu.Backend
  ( Backend(..), Sink(..)
  ) where

import Control.FilterCategory (FilterCategory(..))

import Control.Applicative ()
import Control.Category
import Control.Concurrent (forkIO)
import Control.Monad (liftM2)
import Data.Generics.Aliases (orElse)
import Data.Function (on)
import Data.Monoid (Monoid(..))

import Prelude hiding ((.), id)

data Sink a = Sink
  { sinkConsume :: a -> IO ()
  , sinkMainLoop :: Maybe (IO ())
  , sinkQuitLoop :: IO ()
  }

combineMainLoops :: Maybe (IO ()) -> Maybe (IO ()) -> Maybe (IO ())
combineMainLoops (Just x) (Just y) = Just $ forkIO x >> y
combineMainLoops x y = orElse x y

instance Monoid (Sink a) where
  mempty = Sink (const (return ())) Nothing (return ())
  mappend a b =
    Sink
    { sinkConsume = on (liftM2 (>>)) sinkConsume a b
    , sinkMainLoop = on combineMainLoops sinkMainLoop a b
    , sinkQuitLoop = on (>>) sinkQuitLoop a b
    }
    where

type InBackend p2b b2p = (b2p -> IO ()) -> IO (Sink p2b)

newtype Backend progToBack backToProg =
  Backend
  { runBackend :: InBackend progToBack backToProg
  } -- if Monoid m => Monoid (IO m)
  -- then could use GeneralizedNewtypeDeriving for Monoid

inBackend
  :: (InBackend p0b b0p -> InBackend p1b b1p)
  -> Backend p0b b0p -> Backend p1b b1p
inBackend f = Backend . f . runBackend

instance Monoid (Backend p2b b2p) where
  mempty = Backend . return . return $ mempty
  mappend (Backend x) (Backend y) =
    Backend $ (liftM2 . liftM2) mappend x y

instance Functor (Backend p2b) where
  fmap =
    inBackend . arg . arg
    where
      arg = flip (.)

instance Category Backend where
  id =
    Backend f
    where
      f handler =
        return Sink
        { sinkConsume = handler
        , sinkMainLoop = Nothing
        , sinkQuitLoop = return ()
        }
  Backend left . Backend right =
    Backend f
    where
      f handler = do
        sinkLeft <- left handler
        sinkRight <- right . sinkConsume $ sinkLeft
        return sinkRight
          { sinkMainLoop =
              combineMainLoops
              (sinkMainLoop sinkLeft) (sinkMainLoop sinkRight)
          , sinkQuitLoop =
              sinkQuitLoop sinkLeft >> sinkQuitLoop sinkRight
          }

instance FilterCategory Backend where
  rid = Backend $ runBackend id . maybe (return ())


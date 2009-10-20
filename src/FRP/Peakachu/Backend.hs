module FRP.Peakachu.Backend
  ( Backend(..), Sink(..), mapSink
  ) where

import Control.Applicative ()
import Control.Concurrent (forkIO)
import Control.Monad (liftM2)
import Data.Generics.Aliases (orElse)
import Data.Function (on)
import Data.Monoid (Monoid(..))

data Sink a = Sink
  { sinkConsume :: a -> IO ()
  , sinkMainLoop :: Maybe (IO ())
  , sinkQuitLoop :: IO ()
  }

instance Monoid (Sink a) where
  mempty = Sink (const (return ())) Nothing (return ())
  mappend a b =
    Sink
    { sinkConsume = on (>>) sinkConsume a b
    , sinkMainLoop = on f sinkMainLoop a b
    , sinkQuitLoop = on (>>) sinkQuitLoop a b
    }
    where
      f (Just x) (Just y) = Just $ forkIO x >> y
      f x y = orElse x y

type InBackend p2b b2p = (b2p -> IO ()) -> IO (Sink p2b)

newtype Backend progToBack backToProg =
  Backend
  { runBackend :: InBackend progToBack backToProg
  } -- if Monoid m => Monoid (IO m)
  -- then could use GeneralizedNewtypeDeriving for Monoid

instance Monoid (Backend p2b b2p) where
  mempty = Backend . return . return $ mempty
  mappend (Backend x) (Backend y) =
    Backend $ (liftM2 . liftM2) mappend x y

inBackend
  :: (InBackend p0b b0p -> InBackend p1b b1p)
  -> Backend p0b b0p -> Backend p1b b1p
inBackend f = Backend . f . runBackend

instance Functor (Backend p2b) where
  fmap =
    inBackend . arg . arg
    where
      arg = flip (.)

mapSink :: (a -> Maybe b) -> Backend b i -> Backend a i
mapSink func =
  inBackend . fmap . fmap $ f
  where
    f sink =
      sink
      { sinkConsume = maybe (return ()) (sinkConsume sink) . func }


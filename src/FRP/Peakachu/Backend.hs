{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module FRP.Peakachu.Backend
  ( Backend(..), Sink(..)
  ) where

import Control.FilterCategory (FilterCategory(..))

import Control.Category (Category(..))
import Control.Concurrent (forkIO)
import Control.Instances () -- IO Monoids
import Control.Monad (liftM2)
import Data.DeriveTH (derive, makeFunctor)
import Data.Generics.Aliases (orElse)
import Data.Function (on)
import Data.Monoid (Monoid(..))

import Prelude hiding ((.), id)

data Sink a = Sink
  { sinkConsume :: a -> IO ()
  , sinkInit :: IO ()
  , sinkMainLoop :: Maybe (IO ())
  , sinkQuitLoop :: IO ()
  }

combineMainLoops :: Maybe (IO ()) -> Maybe (IO ()) -> Maybe (IO ())
combineMainLoops (Just x) (Just y) = Just $ forkIO x >> y
combineMainLoops x y = orElse x y

mergeSinks :: Sink a -> Sink b -> (c -> IO ()) -> Sink c
mergeSinks left right consume =
  Sink
  { sinkConsume = consume
  , sinkInit = sinkInit left >> sinkInit right
  , sinkMainLoop =
    combineMainLoops (sinkMainLoop left) (sinkMainLoop right)
  , sinkQuitLoop = sinkQuitLoop left >> sinkQuitLoop right
  }

instance Monoid (Sink a) where
  mempty = Sink (const (return ())) (return ()) Nothing (return ())
  mappend a b =
    mergeSinks a b $
    on (liftM2 (>>)) sinkConsume a b

newtype Backend progToBack backToProg =
  Backend
  { runBackend :: (backToProg -> IO ()) -> IO (Sink progToBack)
  } deriving Monoid
$(derive makeFunctor ''Backend)

instance Category Backend where
  id =
    Backend f
    where
      f handler =
        return mempty { sinkConsume = handler }
  Backend left . Backend right =
    Backend f
    where
      f handler = do
        sinkLeft <- left handler
        sinkRight <- right . sinkConsume $ sinkLeft
        return $ mergeSinks sinkLeft sinkRight (sinkConsume sinkRight)

instance FilterCategory Backend where
  flattenC = Backend (runBackend id . mapM_)
  arrC = (`fmap` id)


{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module FRP.Peakachu.Backend
    ( Backend(..)
    ) where

import Control.FilterCategory (FilterCategory(..))
import FRP.Peakachu.Backend.Internal (Sink(..))

import Control.Category (Category(..))
import Control.Instances () -- IO Monoids
import Data.DeriveTH (derive, makeFunctor)
import Data.Monoid (Monoid(..))

import Prelude hiding ((.), id)

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
                return . Sink (sinkConsume sinkRight) $ mappend (sinkMainLoop sinkLeft) (sinkMainLoop sinkRight)

instance FilterCategory Backend where
    flattenC = Backend (runBackend id . mapM_)
    arrC = (`fmap` id)


{-# OPTIONS -O2 -Wall #-}

module FRP.Peakachu (
  Event, escanl, efilter, emap, empty,
  ereturn, ezip, ezip'
  ) where

import FRP.Peakachu.Internal (
  Event, escanl, efilter, emap, empty)
import Data.Monoid (mappend)

ezip :: Event a -> Event b -> Event (Maybe a, Maybe b)
ezip as bs =
  escanl step (Nothing, Nothing) $ emap Left as `mappend` emap Right bs
  where
    step (_, r) (Left l) = (Just l, r)
    step (l, _) (Right r) = (l, Just r)

ezip' :: Event a -> Event b -> Event (a, b)
ezip' as bs =
  emap m . efilter f $ ezip as bs
  where
    f (Just _, Just _) = True
    f _ = False
    m (Just l, Just r) = (l, r)
    m _ = undefined

ereturn :: a -> Event a
ereturn x = escanl (const id) x empty


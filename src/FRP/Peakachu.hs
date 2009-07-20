{-# OPTIONS -O2 -Wall #-}

module FRP.Peakachu (
  Event, escanl, efilter,
  edrop, ereturn, ezip, ezip'
  ) where

import FRP.Peakachu.Internal (Event, escanl, efilter)
import Data.Monoid (mappend, mempty)

ezip :: Event a -> Event b -> Event (Maybe a, Maybe b)
ezip as bs =
  escanl step (Nothing, Nothing) $ fmap Left as `mappend` fmap Right bs
  where
    step (_, r) (Left l) = (Just l, r)
    step (l, _) (Right r) = (l, Just r)

ezip' :: Event a -> Event b -> Event (a, b)
ezip' as bs =
  fmap m . efilter f $ ezip as bs
  where
    f (Just _, Just _) = True
    f _ = False
    m (Just l, Just r) = (l, r)
    m _ = undefined

ereturn :: a -> Event a
ereturn x = escanl (const id) x mempty

edrop :: Integral i => i -> Event a -> Event a
edrop count =
  fmap snd .
  efilter ((> count) . fst) .
  escanl step (0, undefined)
  where
    step (i, _) x = (i+1, x)

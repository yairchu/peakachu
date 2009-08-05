module FRP.Peakachu (
  Event, SideEffect,
  EventMerge(..), EventZip(..),
  escanl, efilter,
  edrop, ereturn, eMapMaybe,
  ezip, ezip', eZipWith
  ) where

import Data.Maybe (fromJust, isJust)
import Data.Monoid (Monoid(..))
import FRP.Peakachu.Internal (
  Event, SideEffect, escanl, efilter, empty, merge)

-- | Monoid for merging events
newtype EventMerge a = EventMerge { runEventMerge :: Event a }

-- | Monoid for mappending inner Monoids of events
newtype EventZip a = EventZip { runEventZip :: Event a }

instance Functor EventMerge where
  fmap f = EventMerge . fmap f . runEventMerge

instance Monoid (EventMerge a) where
  mempty = EventMerge empty
  mappend a = EventMerge . merge (runEventMerge a) . runEventMerge

instance Functor EventZip where
  fmap f = EventZip . fmap f . runEventZip

instance Monoid a => Monoid (EventZip a) where
  mempty = EventZip $ ereturn mempty
  mappend a = EventZip . eZipWith mappend (runEventZip a) . runEventZip

ezip' :: Event a -> Event b -> Event (Maybe a, Maybe b)
ezip' as bs =
  escanl step (Nothing, Nothing) $ fmap Left as `merge` fmap Right bs
  where
    step (_, r) (Left l) = (Just l, r)
    step (l, _) (Right r) = (l, Just r)

eZipWith :: (a -> b -> c) -> Event a -> Event b -> Event c
eZipWith func as bs =
  fmap m . efilter f $ ezip' as bs
  where
    f (Just _, Just _) = True
    f _ = False
    m (Just l, Just r) = func l r
    m _ = undefined

ezip :: Event a -> Event b -> Event (a, b)
ezip = eZipWith (,)

ereturn :: a -> Event a
ereturn x = escanl (const id) x empty

edrop :: Integral i => i -> Event a -> Event a
edrop count =
  fmap snd .
  efilter ((== 0) . fst) .
  escanl step (count+1, undefined)
  where
    step (0, _) x = (0, x)
    step (i, _) x = (i-1, x)

-- Event is not a MonadPlus so can't use a generic mapMaybe
eMapMaybe :: (a -> Maybe b) -> Event a -> Event b
eMapMaybe func =
  fmap fromJust . efilter isJust . fmap func


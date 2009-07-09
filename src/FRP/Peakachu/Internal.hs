{-# OPTIONS -O2 -Wall #-}

module FRP.Peakachu.Internal (
  Time, Event(..), escanl, efilter, emap
  ) where

import Control.Generator.Producer (Producer, joinP)
import Control.Generator.Folds (filterP, scanlP)
import Control.Generator.Instances ()
import System.Time (ClockTime, getClockTime)

type Time = ClockTime

data Event a = Event { runEvent :: Producer IO (Time, Maybe a) }

escanl :: (a -> b -> a) -> a -> Event b -> Event a
escanl step startVal src =
  Event . joinP $ do
    startTime <- getClockTime
    return . fmap post .
      scanlP process (startTime, startVal, True) $
      runEvent src
  where
    process (_, a, _) (time, b) =
      case b of
        Nothing -> (time, a, False)
        Just x -> (time, step a x, True)
    post (time, _, False) = (time, Nothing)
    post (time, a, True) = (time, Just a)

efilter :: (a -> Bool) -> Event a -> Event a
efilter cond =
  Event . filterP cond' . runEvent
  where
    cond' (_, Nothing) = True
    cond' (_, Just x) = cond x

emap :: (a -> b) -> Event a -> Event b
emap func =
  Event . fmap func' . runEvent
  where
    func' (time, Nothing) = (time, Nothing)
    func' (time, Just x) = (time, Just (func x))


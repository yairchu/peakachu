{-# OPTIONS -O2 -Wall #-}

module FRP.Peakachu.Internal where

import Control.Concurrent.MVar
import Control.Generator
import Control.Generator.Memo
import Control.Generator.ProducerT
import Control.Generator.Tools
import Control.Monad
import Control.Monad.Trans
import Data.Function
import Data.Monoid
import Graphics.UI.GLUT
import System.Time

-- core

type Time = ClockTime

data Event a = Event { runEvent :: Producer IO (Time, Maybe a) }

escanl :: (a -> b -> a) -> a -> Event b -> Event a
escanl step startVal src =
  Event . mmerge $ do
    startTime <- getClockTime
    return . imap post .
      iscanl process (startTime, startVal, True) $
      runEvent src
  where
    process (_, a, _) (time, b) =
      return $ case b of
        Nothing -> (time, a, False)
        Just x -> (time, step a x, True)
    post (time, _, False) = return (time, Nothing)
    post (time, a, True) = return (time, Just a)

efilter :: (a -> Bool) -> Event a -> Event a
efilter cond =
  Event . ifilter cond' . runEvent
  where
    cond' (_, Nothing) = return True
    cond' (_, Just x) = return $ cond x

emap :: (a -> b) -> Event a -> Event b
emap func =
  Event . imap func' . runEvent
  where
    func' (time, Nothing) = return (time, Nothing)
    func' (time, Just x) = return (time, Just (func x))


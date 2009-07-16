{-# OPTIONS -O2 -Wall #-}

module FRP.Peakachu.Internal (
  {-Time,-} Event(..), escanl, efilter, emap
  ) where

import Control.Monad.ListT (ListItem(..), ListT(..))
import Data.List.Class (cons, joinL, scanl)

--import System.Time (ClockTime, getClockTime)
import Prelude hiding (scanl)

-- type Time = ClockTime

newtype Event a = Event { runEvent :: ListT IO [a] }

escanl :: (a -> b -> a) -> a -> Event b -> Event a
escanl step startVal src =
  Event . joinL $ do
    item <- runListT . scanl (scanl step . last) [startVal] $ runEvent src
    return $ case item of
      Nil -> return [startVal]
      Cons items next -> cons (startVal : items) next

efilter :: (a -> Bool) -> Event a -> Event a
efilter cond =
  Event . fmap (filter cond) . runEvent

emap :: (a -> b) -> Event a -> Event b
emap func =
  Event . fmap (fmap func) . runEvent


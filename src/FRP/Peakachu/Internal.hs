{-# OPTIONS -O2 -Wall #-}

module FRP.Peakachu.Internal (
  {-Time,-} Event(..), escanl, efilter, emap,
  makeCallbackEvent, memoEvent
  ) where

import Control.Concurrent.MVar (newMVar, putMVar, takeMVar)
import Control.Monad.ListT (ListItem(..), ListT(..))
import Data.List.Class (cons, joinM, joinL, repeat, scanl)
import Data.Monoid (mempty)

--import System.Time (ClockTime, getClockTime)
import Prelude hiding (repeat, scanl)

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
efilter cond = Event . fmap (filter cond) . runEvent

emap :: (a -> b) -> Event a -> Event b
emap func = Event . fmap (fmap func) . runEvent

memoEvent :: Event a -> IO (Event a)
memoEvent event = do
  var <- newMVar Nothing
  let
    firstRun = do
      item <- runListT $ runEvent event
      case item of
        Nil -> do
          putMVar var $ Just mempty
          return mempty
        Cons [] xs -> do
          rest <- memoEvent $ Event xs
          putMVar var . Just $ runEvent rest
          return . cons [] $ runEvent rest
        Cons x xs -> do
          rest <- memoEvent $ Event xs
          let r = cons x $ runEvent rest
          putMVar var $ Just r
          return r
  return . Event . joinL $ takeMVar var >>= maybe firstRun return

makeCallbackEvent :: IO (Event a, a -> IO ())
makeCallbackEvent = do
  queueVar <- newMVar []
  event <- memoEvent . Event . joinM . repeat $ do
    queue <- takeMVar queueVar
    putMVar queueVar []
    return $ reverse queue
  let
    callback x = do
      queue <- takeMVar queueVar
      putMVar queueVar (x : queue)
  return (event, callback)


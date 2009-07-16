{-# OPTIONS -O2 -Wall #-}

module FRP.Peakachu.Internal (
  {-Time,-} Event(..), escanl, efilter, emap,
  makeCallbackEvent, memoEvent
  ) where

import Control.Concurrent.MVar (newMVar, putMVar, takeMVar)
import Control.Monad.ListT (ListItem(..), ListT(..))
import Data.List.Class (
  cons, joinM, joinL, merge2On, repeat, scanl)
import Data.Monoid (Monoid(..))

import System.Time (ClockTime, getClockTime)
import Prelude hiding (repeat, scanl)

type Time = ClockTime

newtype Event a = Event { runEvent :: ListT IO [(Time, a)] }

instance Monoid (Event a) where
  mempty = Event mempty
  mappend (Event a) (Event b) =
    Event . joinL $ do
      item <- runListT a
      case item of
        Nil -> return b
        Cons valsA restA -> go valsA b restA
    where
      go vals as bs = do
        item <- runListT as
        case item of
          Nil -> return $ cons vals bs
          Cons [] restA ->
            return . cons vals .
            runEvent $ mappend (Event bs) (Event restA)
          Cons valsA restA ->
            go (merge2On fst vals valsA) bs restA

escanl :: (a -> b -> a) -> a -> Event b -> Event a
escanl step startVal src =
  Event . joinL $ do
    now <- getClockTime
    let startItem = (now, startVal)
    item <- runListT . scanl (scanl vstep . last) [startItem] $ runEvent src
    return $ case item of
      Nil -> return [startItem]
      Cons items next -> cons (startItem : items) next
  where
    vstep (_, a) (t, b) = (t, step a b)

efilter :: (a -> Bool) -> Event a -> Event a
efilter cond = Event . fmap (filter (cond . snd)) . runEvent

emap :: (a -> b) -> Event a -> Event b
emap func =
  Event . fmap (fmap f) . runEvent
  where
    f (t, x) = (t, func x)

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
      now <- getClockTime
      queue <- takeMVar queueVar
      putMVar queueVar ((now, x) : queue)
  return (event, callback)


{-# OPTIONS -O2 -Wall #-}

module FRP.Peakachu.Internal (
  Time, Event(..), escanl, efilter,
  makeCallbackEvent, makePollStateEvent, memoEvent
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

instance Functor Event where
  fmap func =
    Event . fmap (fmap f) . runEvent
    where
      f (t, x) = (t, func x)

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

prependItems :: [(Time, a)] -> Event a -> Event a
prependItems pre event =
  Event . joinL $ do
    it <- runListT $ runEvent event
    return $ case it of
      Nil -> return pre
      Cons items next -> cons (pre ++ items) next

escanl :: (a -> b -> a) -> a -> Event b -> Event a
escanl step startVal src =
  Event . joinL $ do
    now <- getClockTime
    let startItems = [(now, startVal)]
    return . runEvent .
      prependItems startItems . Event .
      fmap tail .
      scanl (scanl vstep . last) startItems $
      runEvent src
  where
    vstep (_, a) (t, b) = (t, step a b)

efilter :: (a -> Bool) -> Event a -> Event a
efilter cond = Event . fmap (filter (cond . snd)) . runEvent

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
        Cons x xs -> do
          rest <- memoEvent $ Event xs
          putMVar var . Just . runEvent $ prependItems x rest
          return . cons x $ runEvent rest
  return . Event . joinL $ do
    mx <- takeMVar var
    case mx of
      Nothing -> firstRun
      Just x -> do
        putMVar var mx
        return x

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

makePollStateEvent :: Eq a => IO a -> IO (Event a)
makePollStateEvent poll =
  memoEvent . Event . joinL $ do
    x <- poll
    now <- getClockTime
    return . cons [(now, x)] $ go x
  where
    go prev =
      joinL $ do
        x <- poll
        now <- getClockTime
        return . cons [(now, x) | x /= prev] $ go x


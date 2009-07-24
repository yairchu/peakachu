{-# OPTIONS -O2 -Wall #-}

module FRP.Peakachu.Internal (
  Event(..), escanl, efilter,
  makeCallbackEvent
  ) where

import Control.Concurrent.MVar (
  newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (when)
import Data.Monoid (Monoid(..))

data Event a = Event {
  addHandler :: (a -> IO ()) -> IO (),
  initialValues :: [a]
}

instance Functor Event where
  fmap func event =
    Event {
      addHandler = addHandler event . (. func),
      initialValues = map func (initialValues event)
    }

instance Monoid (Event a) where
  mempty =
    Event {
      addHandler = const (return ()),
      initialValues = []
    }
  mappend x y =
    Event {
      addHandler = \handler -> do
        addHandler x handler
        addHandler y handler,
      initialValues = initialValues x ++ initialValues y
    }

escanl :: (a -> b -> a) -> a -> Event b -> Event a
escanl step startVal event =
  Event {
    addHandler = \handler -> do
      accVar <- newMVar (last initValues)
      let
        srcHandler val = do
          prevAcc <- takeMVar accVar
          let newAcc = step prevAcc val
          putMVar accVar newAcc
          handler newAcc
      addHandler event srcHandler,
    initialValues = initValues
  }
  where
    initValues = scanl step startVal (initialValues event)

efilter :: (a -> Bool) -> Event a -> Event a
efilter cond event =
  Event {
    addHandler = \handler -> do
      let
        srcHandler val =
          when (cond val) (handler val)
      addHandler event srcHandler,
    initialValues = filter cond (initialValues event)
  }

makeCallbackEvent :: IO (Event a, a -> IO ())
makeCallbackEvent = do
  dstHandlersVar <- newMVar []
  let
    srcHandler val =
      mapM_ ($ val) =<< readMVar dstHandlersVar
    event =
      Event {
        addHandler = \handler ->
          takeMVar dstHandlersVar >>=
          putMVar dstHandlersVar . (handler :),
        initialValues = []
      }
  return (event, srcHandler)


module FRP.Peakachu.Internal (
  Event(..), EventEval(..),
  escanl, efilter, ejoin,
  makeCallbackEvent
  ) where

import Control.Concurrent.MVar (
  newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (liftM2, when)
import Data.Monoid (Monoid(..))

data EventEval a = EventEval {
  addHandler :: (a -> IO ()) -> IO (),
  initialValues :: [a]
}

newtype Event a = Event { runEvent :: IO (EventEval a) }

instance Functor EventEval where
  fmap func event =
    EventEval {
      addHandler = addHandler event . (. func),
      initialValues = map func (initialValues event)
    }

instance Monoid (EventEval a) where
  mempty =
    EventEval {
      addHandler = const (return ()),
      initialValues = []
    }
  mappend x y =
    EventEval {
      addHandler = \handler -> do
        addHandler x handler
        addHandler y handler,
      initialValues = (initialValues x) ++ (initialValues y)
    }

instance Functor Event where
  fmap func = Event . fmap (fmap func) . runEvent

instance Monoid (Event a) where
  mempty = Event $ return mempty
  mappend x y = Event $ liftM2 mappend (runEvent x) (runEvent y)

escanl :: (a -> b -> a) -> a -> Event b -> Event a
escanl step startVal event =
  Event $ do
    ev <- runEvent event
    let
      initValues = scanl step startVal (initialValues ev)
    accVar <- newMVar (last initValues)
    let
      srcHandler handler val = do
        prevAcc <- takeMVar accVar
        let newAcc = step prevAcc val
        putMVar accVar newAcc
        handler newAcc
    return EventEval {
      addHandler = addHandler ev . srcHandler,
      initialValues = initValues
      }

efilter :: (a -> Bool) -> Event a -> Event a
efilter cond event =
  Event $ do
    ev <- runEvent event
    let
      srcHandler handler val =
        when (cond val) (handler val)
    return EventEval {
      addHandler = addHandler ev . srcHandler,
      initialValues = filter cond (initialValues ev)
      }

makeCallbackEvent :: IO (Event a, a -> IO ())
makeCallbackEvent = do
  dstHandlersVar <- newMVar []
  let
    srcHandler val =
      mapM_ ($ val) =<< readMVar dstHandlersVar
    ev =
      EventEval {
        addHandler = \handler ->
          takeMVar dstHandlersVar >>=
          putMVar dstHandlersVar . (handler :),
        initialValues = []
      }
  return (Event (return ev), srcHandler)

ejoin :: Event (IO a) -> Event a
ejoin event =
  Event $ do
    ev <- runEvent event
    let
      srcHandler handler val = val >>= handler
    initVals <- sequence $ initialValues ev
    return EventEval {
      addHandler = addHandler ev . srcHandler,
      initialValues = initVals
      }


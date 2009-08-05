module FRP.Peakachu.Internal (
  Event(..), EventEval(..), SideEffect(..),
  escanl, efilter, ejoin, empty, merge,
  executeSideEffect,
  makeCallbackEvent
  ) where

import Control.Concurrent.MVar (
  newMVar, modifyMVar_, putMVar, readMVar, takeMVar)
import Control.Monad (when)
import Control.Monad.Instances ()
import Control.Applicative (liftA2)
import Control.Instances () -- Conal's TypeCompose instances
import Data.Monoid (Monoid(..))

data EventEval a = EventEval {
  addHandler :: (a -> IO ()) -> IO (),
  initialValues :: [a]
}

newtype Event a = Event { runEvent :: IO (EventEval a) }

newtype SideEffect = SideEffect { runSideEffect :: Event (IO ()) }

instance Functor Event where
  fmap func event =
    Event $ do
      ev <- runEvent event
      return EventEval {
        addHandler = addHandler ev . (. func),
        initialValues = map func (initialValues ev)
      }

empty :: Event a
empty =
 Event . return $ EventEval {
   addHandler = const . return $ (),
   initialValues = []
 }

merge :: Event a -> Event a -> Event a
merge ex ey =
  Event $ do
    x <- runEvent ex
    y <- runEvent ey
    return EventEval {
      addHandler    = addHandler    x `mappend` addHandler y,
      initialValues = initialValues x `mappend` initialValues y
    }

-- This is the natural Monoid if SideEffect was a MergeEvent, but
-- MergeEvent is unusable here
instance Monoid SideEffect where
  mempty = SideEffect empty
  mappend x y = SideEffect $ runSideEffect x `merge` runSideEffect y

escanl :: (a -> b -> a) -> a -> Event b -> Event a
escanl step startVal event =
  Event $ do
    ev <- runEvent event
    let initValues = scanl step startVal (initialValues ev)
    accVar <- newMVar $ last initValues
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
    return EventEval {
      addHandler = addHandler ev . liftA2 when cond,
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
        addHandler = modifyMVar_ dstHandlersVar . (return .) . (:),
        initialValues = []
      }
  return (Event (return ev), srcHandler)

ejoin :: Event (IO a) -> Event a
ejoin event =
  Event $ do
    ev <- runEvent event
    initVals <- sequence $ initialValues ev
    return EventEval {
      addHandler = addHandler ev . (=<<),
      initialValues = initVals
    }

executeSideEffect :: SideEffect -> IO ()
executeSideEffect effect = do
  ev <- runEvent . ejoin . runSideEffect $ effect
  addHandler ev . const . return $ ()

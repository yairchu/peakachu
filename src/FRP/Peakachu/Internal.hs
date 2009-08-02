module FRP.Peakachu.Internal (
  Event(..), EventEval(..), SideEffect(..),
  escanl, efilter, ejoin, empty, merge,
  executeSideEffect,
  makeCallbackEvent
  ) where

import Control.Concurrent.MVar (
  newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (when)
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
   addHandler = const (return ()),
   initialValues = []
 }

merge :: Event a -> Event a -> Event a
merge ex ey =
  Event $ do
    x <- runEvent ex
    y <- runEvent ey
    return EventEval {
      addHandler = \handler -> do
        addHandler x handler
        addHandler y handler,
      initialValues = initialValues x ++ initialValues y
    }

-- there is only one Monoid for SideEffect that makes sense
instance Monoid SideEffect where
  mempty = SideEffect empty
  mappend x y = SideEffect $ merge (runSideEffect x) (runSideEffect y)

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

executeSideEffect :: SideEffect -> IO ()
executeSideEffect effect = do
  ev <- runEvent . ejoin . runSideEffect $ effect
  addHandler ev . const $ return ()


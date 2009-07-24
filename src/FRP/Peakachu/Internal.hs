{-# OPTIONS -O2 -Wall #-}

module FRP.Peakachu.Internal (
  Event(..), escanl, efilter,
  makeCallbackEvent, addHandler
  ) where

import Control.Concurrent.MVar (
  MVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (when)
import Data.Monoid (Monoid(..))

type Handler a = a -> IO ()

newtype Event a = Event {
  runEvent :: IO (MVar [Handler a])
}

addHandler :: Handler a -> Event a -> IO ()
addHandler handler event = do
  var <- runEvent event
  putMVar var . (handler :) =<< takeMVar var

instance Functor Event where
  fmap func event = Event $ do
    dstHandlersVar <- newMVar []
    let
      srcHandler val =
        mapM_ ($ func val) =<< readMVar dstHandlersVar
    addHandler srcHandler event
    return dstHandlersVar

instance Monoid (Event a) where
  mempty = Event $ newMVar []
  mappend a b = Event $ do
    dstHandlersVar <- newMVar []
    let
      srcHandler val =
        mapM_ ($ val) =<< readMVar dstHandlersVar
    addHandler srcHandler a
    addHandler srcHandler b
    return dstHandlersVar

escanl :: (a -> b -> a) -> a -> Event b -> Event a
escanl step startVal event = Event $ do
  -- TODO : yield start value
  dstHandlersVar <- newMVar []
  accVar <- newMVar startVal
  let
    srcHandler val = do
      prevAcc <- takeMVar accVar
      let newAcc = step prevAcc val
      putMVar accVar newAcc
      mapM_ ($ newAcc) =<< readMVar dstHandlersVar
  addHandler srcHandler event
  return dstHandlersVar

efilter :: (a -> Bool) -> Event a -> Event a
efilter cond event = Event $ do
  dstHandlersVar <- newMVar []
  let
    srcHandler val =
      when (cond val) $
      mapM_ ($ val) =<< readMVar dstHandlersVar
  addHandler srcHandler event
  return dstHandlersVar

makeCallbackEvent :: IO (Event a, a -> IO ())
makeCallbackEvent = do
  dstHandlersVar <- newMVar []
  let
    srcHandler val =
      mapM_ ($ val) =<< readMVar dstHandlersVar
  return (Event (return dstHandlersVar), srcHandler)


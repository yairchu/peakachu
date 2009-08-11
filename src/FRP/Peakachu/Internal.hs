module FRP.Peakachu.Internal (
  Event(..), inEvent, SideEffect(..),
  escanl, efilter, empty, merge,
  executeSideEffect,
  makeCallbackEvent,
  mkEvent, inMkEvent, setHandler, inEvent2
  ) where

import Control.Applicative (liftA2)
import Control.Concurrent.MVar (
  MVar, newMVar, modifyMVar_, putMVar, readMVar, takeMVar)
import Control.Monad (join, when)
import Control.Monad.Cont (ContT(..))
import Control.Monad.Cont.Monoid (inContT)
import Control.Monad.Instances ()
import Control.Monad.Trans (lift)
import Control.Instances () -- Conal's TypeCompose instances
import Control.SECombinator (argument, result)
import Data.Monoid (Monoid(..))

type InEvent = ContT () IO

newtype Event a = Event { runEvent :: InEvent a }

inEvent :: (InEvent a -> InEvent b) -> Event a -> Event b
inEvent func = Event . func . runEvent

mkEvent :: ((a -> IO ()) -> IO ()) -> Event a
mkEvent = Event . ContT

-- setHandler is the inverse of mkEvent
setHandler :: Event a -> (a -> IO ()) -> IO ()
setHandler = runContT . runEvent

inMkEvent ::
  (((a -> IO ()) -> IO ()) -> (b -> IO ()) -> IO ()) ->
  Event a -> Event b
inMkEvent = inEvent . inContT

inEvent2 ::
  (InEvent a -> InEvent b -> InEvent c) ->
  Event a -> Event b -> Event c
inEvent2 = result inEvent . argument runEvent

newtype SideEffect = SideEffect { runSideEffect :: Event (IO ()) }

instance Functor Event where
  fmap = inEvent . fmap

empty :: Event a
empty = Event mempty

merge :: Event a -> Event a -> Event a
merge = inEvent2 mappend

-- This is the natural Monoid if SideEffect was a MergeEvent, but
-- MergeEvent is unusable here
instance Monoid SideEffect where
  mempty = SideEffect empty
  mappend x y = SideEffect $ runSideEffect x `merge` runSideEffect y

-- cons should actually be for "Applicative m", not Monad
-- but there's no Applicative instance for "ContT () IO"..
cons :: (Monad m, Monoid (m a)) => a -> m a -> m a
cons = mappend . return

escanl :: (a -> b -> a) -> a -> Event b -> Event a
escanl step startVal event =
  Event $ do
    accVar <- lift $ newMVar startVal
    let
      srcHandler handler val =
        takeMVar accVar >>=
        liftA2 mappend (putMVar accVar) handler . (`step` val)
    cons startVal . runEvent $ inMkEvent (argument srcHandler) event

efilter :: (a -> Bool) -> Event a -> Event a
efilter = inMkEvent . argument . liftA2 when

modifyMVarPure :: MVar a -> (a -> a) -> IO ()
modifyMVarPure mvar = modifyMVar_ mvar . result return

setMVar :: MVar a -> a -> IO ()
setMVar mvar = modifyMVarPure mvar . const

makeCallbackEvent :: IO (Event a, a -> IO ())
makeCallbackEvent = do
  dstHandlersVar <- newMVar []
  let
    srcHandler val =
      mapM_ ($ val) =<< readMVar dstHandlersVar
    event =
      mkEvent $
      modifyMVarPure dstHandlersVar . (:)
  return (event, srcHandler)

-- how executeSideEffect works:
-- only after all side-effects have initialized,
-- perform their start-up effects,
-- so that all event handlers had their oppurtunity to subscribe to the events
executeSideEffect :: SideEffect -> IO ()
executeSideEffect effect = do
  startedVar <- newMVar False
  startQueue <- newMVar mempty -- start-up effects
  let
    handler action = do
      started <- readMVar startedVar
      if started
        then action
        else modifyMVarPure startQueue (mappend action)
  setHandler (runSideEffect effect) handler
  setMVar startedVar True
  join (readMVar startQueue) -- run start-up effects


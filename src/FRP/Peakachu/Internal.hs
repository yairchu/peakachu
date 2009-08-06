module FRP.Peakachu.Internal (
  Event(..), inEvent, SideEffect(..),
  escanl, efilter, ejoin, empty, merge,
  executeSideEffect,
  makeCallbackEvent,
  argument
  ) where

import Control.Concurrent.MVar (
  newMVar, modifyMVar_, putMVar, readMVar, takeMVar)
import Control.Monad (when)
import Control.Monad.Instances ()
import Control.Applicative (liftA2)
import Control.Instances () -- Conal's TypeCompose instances
import Data.Monoid (Monoid(..))

type InEvent a = (a -> IO ()) -> IO ()
newtype Event a = Event { runEvent :: InEvent a }
inEvent :: (InEvent a -> InEvent b) -> Event a -> Event b
inEvent func = Event . func . runEvent

newtype SideEffect = SideEffect { runSideEffect :: Event (IO ()) }

-- from Conal's semantic editor combinators
result :: (a -> b) -> (c -> a) -> (c -> b)
result = (.)
argument :: (a -> b) -> (b -> c) -> (a -> c)
argument = flip (.)

instance Functor Event where
  fmap = inEvent . argument . argument

empty :: Event a
empty = Event (const mempty)

inEvent2 ::
  (InEvent a -> InEvent b -> InEvent c) ->
  Event a -> Event b -> Event c
inEvent2 = result inEvent . argument runEvent

merge :: Event a -> Event a -> Event a
merge = inEvent2 mappend

-- This is the natural Monoid if SideEffect was a MergeEvent, but
-- MergeEvent is unusable here
instance Monoid SideEffect where
  mempty = SideEffect empty
  mappend x y = SideEffect $ runSideEffect x `merge` runSideEffect y

escanl :: (a -> b -> a) -> a -> Event b -> Event a
escanl step startVal event =
  Event $ \handler -> do
    handler startVal
    accVar <- newMVar startVal
    let
      srcHandler val = do
        prevAcc <- takeMVar accVar
        let newAcc = step prevAcc val
        putMVar accVar newAcc
        handler newAcc
    runEvent event srcHandler

efilter :: (a -> Bool) -> Event a -> Event a
efilter = inEvent . argument . liftA2 when

makeCallbackEvent :: IO (Event a, a -> IO ())
makeCallbackEvent = do
  dstHandlersVar <- newMVar []
  let
    srcHandler val =
      mapM_ ($ val) =<< readMVar dstHandlersVar
    addHandler =
      modifyMVar_ dstHandlersVar . (return .) . (:)
  return (Event addHandler, srcHandler)

ejoin :: Event (IO a) -> Event a
ejoin = inEvent $ argument (=<<)

executeSideEffect :: SideEffect -> IO ()
executeSideEffect =
  (`runEvent` const mempty) . runSideEffect


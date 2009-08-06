module FRP.Peakachu.Internal (
  Event(..), SideEffect(..),
  escanl, efilter, ejoin, empty, merge,
  executeSideEffect,
  makeCallbackEvent,
  argument, eventBoo -- NEED BETTER NAME!
  ) where

import Control.Concurrent.MVar (
  newMVar, modifyMVar_, putMVar, readMVar, takeMVar)
import Control.Monad (when)
import Control.Monad.Instances ()
import Control.Applicative (liftA2)
import Control.Instances () -- Conal's TypeCompose instances
import Data.Monoid (Monoid(..))

newtype Event a = Event { runEvent :: (a -> IO ()) -> IO () }

newtype SideEffect = SideEffect { runSideEffect :: Event (IO ()) }

-- there is a name for this and it isn't boo.
-- what is the name? it's like in matrices "boo A B = A*B*(A^-1)"..
-- I remember there's a name for this..
eventBoo :: (((a -> IO ()) -> IO ()) -> ((b -> IO ()) -> IO ())) -> Event a -> Event b
eventBoo func = Event . func . runEvent

-- from Conal's semantic editor combinators
argument :: (a -> b) -> (b -> c) -> (a -> c)
argument = flip (.)

instance Functor Event where
  fmap = eventBoo . argument . argument

empty :: Event a
empty = Event (const mempty)

merge :: Event a -> Event a -> Event a
merge x y = Event $ runEvent x `mappend` runEvent y

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
efilter = eventBoo . argument . liftA2 when

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
ejoin = eventBoo $ argument (=<<)

executeSideEffect :: SideEffect -> IO ()
executeSideEffect =
  (`runEvent` const mempty) . runSideEffect


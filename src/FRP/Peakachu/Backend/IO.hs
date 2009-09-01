module FRP.Peakachu.Backend.IO (
  mkCallbackEvent, mkEffectfulFunc, mkSideEffect, liftForkIO
  ) where

import Control.Concurrent.MVar.YC (modifyMVarPure)
import FRP.Peakachu (EffectfulFunc)
import FRP.Peakachu.Internal (SideEffect(..), Event(..))

import Control.Applicative ((<$))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newMVar, readMVar)
import Control.Monad.Cont (ContT(..))

mkCallbackEvent :: IO (Event a, a -> IO ())
mkCallbackEvent = do
  dstHandlersVar <- newMVar []
  let
    srcHandler val =
      mapM_ ($ val) =<< readMVar dstHandlersVar
    event =
      Event $
      modifyMVarPure dstHandlersVar . (:)
  return (event, srcHandler)

mkEffectfulFunc ::
  (a -> ContT () IO b) -> IO (EffectfulFunc a b c)
mkEffectfulFunc go = do
  (event, callback) <- mkCallbackEvent
  let
    f (input, other) =
      runContT (go input) (callback . flip (,) other)
  return (SideEffect . fmap f, event)

mkSideEffect :: (a -> IO ()) -> Event a -> SideEffect
mkSideEffect func = SideEffect . fmap func

liftForkIO :: ContT () IO ()
liftForkIO =
  ContT $ \rest -> do
    forkIO $ () <$ rest ()
    return ()



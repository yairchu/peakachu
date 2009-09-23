module FRP.Peakachu.Backend.IO (
  mkCallbackEvent, mkEffectFunc, mkSideEffect, liftForkIO
  ) where

import Control.Concurrent.MVar.YC (modifyMVarPure)
import FRP.Peakachu (EffectFunc(..))
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

mkEffectFunc ::
  (a -> ContT () IO b) -> IO (EffectFunc a b c)
mkEffectFunc go = do
  (event, callback) <- mkCallbackEvent
  let
    f (input, other) =
      runContT (go input) (callback . flip (,) other)
  return $ EffectFunc (mkSideEffect f) event

mkSideEffect :: (a -> IO ()) -> Event a -> SideEffect
mkSideEffect func = SideEffect . fmap func

liftForkIO :: ContT () IO ()
liftForkIO =
  ContT $ \rest -> do
    forkIO $ () <$ rest ()
    return ()



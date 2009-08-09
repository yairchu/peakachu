module FRP.Peakachu.Backend.IO (
  mkEffectfulFunc, mkSideEffect, liftForkIO
  ) where

import FRP.Peakachu (EffectfulFunc, Event)
import FRP.Peakachu.Internal (
  SideEffect(..), makeCallbackEvent)

import Control.Applicative ((<$))
import Control.Concurrent (forkIO)
import Control.Monad.Cont (ContT(..))

mkEffectfulFunc ::
  (a -> ContT () IO b) -> IO (EffectfulFunc a b c)
mkEffectfulFunc go = do
  (event, callback) <- makeCallbackEvent
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



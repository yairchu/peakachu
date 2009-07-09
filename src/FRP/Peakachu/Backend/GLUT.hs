{-# OPTIONS -O2 -Wall #-}

module FRP.Peakachu.Backend.GLUT (
  Image(..), glKeyboardMouseEvents, glutRun
  ) where

import Control.Generator.Consumer (evalConsumerT, next, consumeRestM)
import Control.Concurrent.MVar (newMVar, putMVar, takeMVar)
import Control.Generator.Memo (memo)
import Control.Generator.ProducerT (produce, yield)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.Function (fix)
import Data.Monoid (Monoid(..))
import FRP.Peakachu.Internal (Event(..))
import Graphics.UI.GLUT (
  ($=), ClearBuffer(..), Key(..), KeyState(..),
  Modifiers, Position,
  displayCallback, keyboardMouseCallback, idleCallback,
  clear, flush, mainLoop)
import System.Time (getClockTime)

data Image = Image (IO ())

instance Monoid Image where
  mempty = Image $ return ()
  mappend (Image a) (Image b) = Image $ a >> b

glKeyboardMouseEvents :: IO (Event Char)
glKeyboardMouseEvents = do
  queueVar <- newMVar []
  let
    callback :: Key -> KeyState -> Modifiers -> Position -> IO ()
    callback (Char c) Down _ _ = do
      t <- getClockTime
      queue <- takeMVar queueVar
      putMVar queueVar $ queue ++ [(t, Just c)]
    callback _ _ _ _ = return ()
  keyboardMouseCallback $= Just callback
  r <- memo . produce . forever $ do
    queue <- liftIO $ takeMVar queueVar
    case queue of
      [] -> do
        t <- liftIO getClockTime
        liftIO $ putMVar queueVar []
        yield (t, Nothing)
      x : xs -> do
        liftIO $ putMVar queueVar xs
        yield x
  return $ Event r

glutRun :: Event Image -> IO ()
glutRun program = do
  (`evalConsumerT` runEvent program) . fix $ \rest -> do
    Just mx <- next
    case snd mx of
      Nothing -> return ()
      Just (Image image) -> liftIO $ do
        clear [ ColorBuffer ]
        image
        flush
    liftIO . (idleCallback $=) . Just =<< consumeRestM rest
  displayCallback $= return ()
  mainLoop


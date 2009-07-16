{-# OPTIONS -O2 -Wall #-}

module FRP.Peakachu.Backend.GLUT (
  Image(..), glKeyboardMouseEvents, glutRun
  ) where

import Control.Concurrent.MVar (newMVar, putMVar, takeMVar)
import Control.Monad (when)
import Control.Monad.Consumer (consumeRestM, evalConsumerT, next)
import Control.Monad.Trans (liftIO)
import Data.Function (fix)
import Data.List.Class (joinM, repeat)
import Data.Monoid (Monoid(..))
import FRP.Peakachu.Internal (Event(..))
import Graphics.UI.GLUT (
  ($=), ClearBuffer(..), Key(..), KeyState(..),
  Modifiers, Position,
  displayCallback, keyboardMouseCallback, idleCallback,
  clear, flush, mainLoop, leaveMainLoop)
--import System.Time (getClockTime)
import Prelude hiding (repeat)

data Image = Image { runImage :: IO ()}

instance Monoid Image where
  mempty = Image $ return ()
  mappend (Image a) (Image b) = Image $ a >> b

glKeyboardMouseEvents :: IO (Event Char)
glKeyboardMouseEvents = do
  queueVar <- newMVar []
  let
    callback :: Key -> KeyState -> Modifiers -> Position -> IO ()
    callback (Char c) Down _ _ = do
      --t <- getClockTime
      queue <- takeMVar queueVar
      putMVar queueVar (c:queue) --queue ++ [(t, Just c)]
    callback _ _ _ _ = return ()
  keyboardMouseCallback $= Just callback
  return . Event . joinM . repeat $ do
    queue <- takeMVar queueVar
    putMVar queueVar []
    return $ reverse queue

glutRun :: Event Image -> IO ()
glutRun program = do
  (`evalConsumerT` runEvent program) . fix $ \rest -> do
    mx <- next
    case mx of
      Nothing -> liftIO $ leaveMainLoop
      Just items -> do
        when (not (null items)) . liftIO $ do
          clear [ ColorBuffer ]
          runImage $ last items
          flush
        liftIO . (idleCallback $=) . Just =<< consumeRestM rest
  displayCallback $= return ()
  mainLoop


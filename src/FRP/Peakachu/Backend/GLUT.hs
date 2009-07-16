{-# OPTIONS -O2 -Wall #-}

module FRP.Peakachu.Backend.GLUT (
  Image(..), glutRun,
  mouseMotionEvent,
  glKeyboardMouseEvent, glMotionEvent, glPassiveMotionEvent
  ) where

import Control.Monad (unless)
import Control.Monad.Consumer (consumeRestM, evalConsumerT, next)
import Control.Monad.Trans (liftIO)
import Data.Function (fix)
import Data.Monoid (Monoid(..))
import Data.List.Class (joinM)
import Foreign (unsafePerformIO)
import FRP.Peakachu.Internal (Event(..), makeCallbackEvent)
import Graphics.UI.GLUT (
  ($=), ClearBuffer(..), Key(..), KeyState(..),
  Modifiers, Position(..), GLfloat, Size(..),
  createWindow, getArgsAndInitialize,
  displayCallback, idleCallback, keyboardMouseCallback,
  motionCallback, passiveMotionCallback,
  get, windowSize,
  clear, flush, mainLoop, leaveMainLoop)
import Prelude hiding (repeat)

data Image = Image { runImage :: IO ()}

instance Monoid Image where
  mempty = Image $ return ()
  mappend (Image a) (Image b) = Image $ a >> b

glKeyboardMouseEvent :: Event (Key, KeyState, Modifiers, Position)
glKeyboardMouseEvent =
  unsafePerformIO $ do
    (event, callback) <- makeCallbackEvent
    let cb a b c d = callback (a, b, c, d)
    keyboardMouseCallback $= Just cb
    return event

glMotionEvent :: Event Position
glMotionEvent =
  unsafePerformIO $ do
    (event, callback) <- makeCallbackEvent
    motionCallback $= Just callback
    return event

glPassiveMotionEvent :: Event Position
glPassiveMotionEvent =
  unsafePerformIO $ do
    (event, callback) <- makeCallbackEvent
    passiveMotionCallback $= Just callback
    return event

mouseMotionEvent :: Event (GLfloat, GLfloat)
mouseMotionEvent =
  Event . joinM . fmap f . runEvent $
  mappend glMotionEvent glPassiveMotionEvent
  where
    f :: [(t, Position)] -> IO [(t, (GLfloat, GLfloat))]
    f items = do
      Size sx sy <- get windowSize
      let
        ff (when, Position px py) =
          (when,
           (2 * fromIntegral px / fromIntegral sx - 1
           ,(-2) * fromIntegral py / fromIntegral sy + 1))
      return $ fmap ff items

glutRun :: Event Image -> IO ()
glutRun program = do
  _ <- getArgsAndInitialize
  createWindow "test"
  (`evalConsumerT` runEvent program) . fix $ \rest -> do
    mx <- next
    case mx of
      Nothing -> liftIO leaveMainLoop
      Just items -> do
        unless (null items) . liftIO $ do
          clear [ ColorBuffer ]
          runImage . snd $ last items
          flush
        liftIO . (idleCallback $=) . Just =<< consumeRestM rest
  displayCallback $= return ()
  mainLoop


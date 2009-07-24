{-# OPTIONS -O2 -Wall #-}

module FRP.Peakachu.Backend.GLUT (
  Image(..), glutRun,
  mouseMotionEvent,
  glKeyboardMouseEvent, glMotionEvent, glPassiveMotionEvent,
  ) where

import Control.Monad (unless)
import Control.Monad.ListT (ListT(..), ListItem(..))
import Data.Monoid (Monoid(..))
import Foreign (unsafePerformIO)
import FRP.Peakachu (ereturn, ezip')
import FRP.Peakachu.Internal (
  Event(..), Time, makeCallbackEvent, makePollStateEvent)
import Graphics.UI.GLUT (
  ($=), ($~), ClearBuffer(..), Key(..), KeyState(..),
  Modifiers, Position(..), GLfloat, Size(..),
  DisplayMode(..), initialDisplayMode, swapBuffers,
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

windowSizeEvent :: Event Size
windowSizeEvent =
  unsafePerformIO .
  makePollStateEvent $
  get windowSize

mouseMotionEvent :: Event (GLfloat, GLfloat)
mouseMotionEvent =
  mappend (ereturn (0, 0)) . -- is there a way to get the initial mouse position?
  fmap f .
  ezip' windowSizeEvent $
  mappend glMotionEvent glPassiveMotionEvent
  where
    f ((Size sx sy), (Position px py)) = (r sx px, - r sy py)
    r sa pa = 2 * fromIntegral pa / fromIntegral sa - 1

glutIdleCallback :: ListT IO [(Time, Image)] -> IO ()
glutIdleCallback program = do
  item <- runListT program
  case item of
    Nil -> leaveMainLoop
    Cons items rest -> do
      unless (null items) $ do
        clear [ ColorBuffer ]
        runImage . snd $ last items
        swapBuffers
        flush
      idleCallback $= Just (glutIdleCallback rest)

glutRun :: Event Image -> IO ()
glutRun program = do
  _ <- getArgsAndInitialize
  initialDisplayMode $~ (DoubleBuffered:)
  createWindow "test"
  idleCallback $= Just (glutIdleCallback (runEvent program))
  displayCallback $= return ()
  mainLoop


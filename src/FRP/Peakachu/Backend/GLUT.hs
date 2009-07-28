module FRP.Peakachu.Backend.GLUT (
  Image(..), UI(..), run
  ) where

import Data.Monoid (Monoid(..))
import FRP.Peakachu (ereturn)
import FRP.Peakachu.Internal (
  Event(..), EventEval(..), SideEffect,
  executeSideEffect, makeCallbackEvent)
import Graphics.UI.GLUT (
  ($=), ($~), SettableStateVar, get,
  ClearBuffer(..), Key(..), KeyState(..),
  Modifiers, Position(..), GLfloat, Size(..),
  DisplayMode(..), initialDisplayMode, swapBuffers,
  createWindow, getArgsAndInitialize,
  displayCallback, keyboardMouseCallback,
  motionCallback, passiveMotionCallback,
  windowSize,
  clear, flush, mainLoop)
import Prelude hiding (repeat)

data Image = Image { runImage :: IO ()}

instance Monoid Image where
  mempty = Image $ return ()
  mappend (Image a) (Image b) = Image $ a >> b

data UI = UI {
  mouseMotionEvent :: Event (GLfloat, GLfloat),
  glutKeyboardMouseEvent :: Event (Key, KeyState, Modifiers, Position)
  }

makeCallbackEvent' ::
  SettableStateVar (Maybe b) ->
  ((a -> IO ()) -> b) ->
  IO (Event a)
makeCallbackEvent' callbackVar trans = do
  (event, callback) <- makeCallbackEvent
  callbackVar $= Just (trans callback)
  return event

createUI :: IO UI
createUI = do
  Size sx sy <- get windowSize
  glutMotionEvent <- makeCallbackEvent' motionCallback id
  glutPassiveMotionEvent <- makeCallbackEvent' passiveMotionCallback id
  glutKeyboardMouseE <-
    makeCallbackEvent' keyboardMouseCallback $
    \cb a b c d -> cb (a,b,c,d)
  let
    pixel2gl (Position px py) = (p2g sx px, - p2g sy py)
    p2g sa pa = 2 * fromIntegral pa / fromIntegral sa - 1
  return UI {
    glutKeyboardMouseEvent = glutKeyboardMouseE,
    mouseMotionEvent =
      mappend (ereturn (0, 0)) . -- is there a way to get the initial mouse position?
      fmap pixel2gl $
      mappend glutMotionEvent glutPassiveMotionEvent
  }

draw :: Image -> IO ()
draw image = do
  clear [ ColorBuffer ]
  runImage image
  swapBuffers
  flush

run :: (UI -> Event Image) -> SideEffect -> IO ()
run programDesc effect = do
  _ <- getArgsAndInitialize
  initialDisplayMode $~ (DoubleBuffered:)
  createWindow "test"
  displayCallback $= return ()
  program <- runEvent =<< fmap programDesc createUI
  mapM_ draw (initialValues program)
  addHandler program draw
  executeSideEffect effect
  mainLoop


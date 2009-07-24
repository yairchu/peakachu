{-# OPTIONS -O2 -Wall #-}

module FRP.Peakachu.Backend.GLUT (
  Image(..), UI(..), run
  ) where

import Control.Monad (unless)
import Control.Monad.ListT (ListT(..), ListItem(..))
import Data.Monoid (Monoid(..))
import FRP.Peakachu (ereturn, ezip')
import FRP.Peakachu.Internal (
  Event(..), Time, makeCallbackEvent, makePollStateEvent)
import Graphics.UI.GLUT (
  ($=), ($~), SettableStateVar,
  ClearBuffer(..), Key(..), KeyState(..),
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

data UI = UI {
  windowSizeEvent :: Event Size,
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
  windowSizeE <- makePollStateEvent (get windowSize)
  glutMotionEvent <- makeCallbackEvent' motionCallback id
  glutPassiveMotionEvent <- makeCallbackEvent' passiveMotionCallback id
  glutKeyboardMouseE <-
    makeCallbackEvent' keyboardMouseCallback $
    \cb a b c d -> cb (a,b,c,d)
  return UI {
    windowSizeEvent = windowSizeE,
    glutKeyboardMouseEvent = glutKeyboardMouseE,
    mouseMotionEvent =
      mappend (ereturn (0, 0)) . -- is there a way to get the initial mouse position?
      fmap pixel2gl .
      ezip' windowSizeE $
      mappend glutMotionEvent glutPassiveMotionEvent
  }
  where
    pixel2gl ((Size sx sy), (Position px py)) = (p2g sx px, - p2g sy py)
    p2g sa pa = 2 * fromIntegral pa / fromIntegral sa - 1

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

run :: (UI -> Event Image) -> IO ()
run program = do
  _ <- getArgsAndInitialize
  initialDisplayMode $~ (DoubleBuffered:)
  createWindow "test"
  (idleCallback $=) .
    Just . glutIdleCallback .
    runEvent . program =<< createUI
  displayCallback $= return ()
  mainLoop


module FRP.Peakachu.Backend.GLUT (
  GlutEvent(..), Image(..), UI,
  glutIdleEvent, glutKeyboardMouseEvent, mouseMotionEvent,
  run
  ) where

import Data.Monoid (Monoid(..))
import FRP.Peakachu (EventMerge(..), ereturn, eMapMaybe)
import FRP.Peakachu.Internal (
  Event(..), EventEval(..), SideEffect,
  executeSideEffect, makeCallbackEvent)
import Graphics.UI.GLUT (
  ($=), ($~), SettableStateVar, get,
  ClearBuffer(..), Key(..), KeyState(..),
  Modifiers, Position(..), Size(..),
  DisplayMode(..), initialDisplayMode, swapBuffers,
  createWindow, getArgsAndInitialize,
  displayCallback, idleCallback,
  keyboardMouseCallback,
  motionCallback, passiveMotionCallback,
  windowSize,
  clear, flush, mainLoop)

data Image = Image { runImage :: IO ()}

instance Monoid Image where
  mempty = Image $ return ()
  mappend (Image a) (Image b) = Image $ a >> b

data GlutEvent =
  IdleEvent
  | MouseMotionEvent Float Float
  | KeyboardMouseEvent Key KeyState Modifiers Position

type UI = Event GlutEvent

glutCallbackEvent ::
  SettableStateVar (Maybe b) ->
  ((a -> IO ()) -> b) ->
  IO (Event a)
glutCallbackEvent callbackVar trans = do
  (event, callback) <- makeCallbackEvent
  callbackVar $= Just (trans callback)
  return event

createUI :: IO UI
createUI = do
  Size sx sy <- get windowSize
  let
    pixel2gl cb (Position px py) =
      cb $ MouseMotionEvent (p2g sx px) (- p2g sy py)
    p2g sa pa = 2 * fromIntegral pa / fromIntegral sa - 1
    kbMouse cb key keyState mods =
      cb . KeyboardMouseEvent key keyState mods
  fmap (runEventMerge . mconcat . map EventMerge) $ sequence
    [ return (ereturn (MouseMotionEvent 0 0))
    , glutCallbackEvent motionCallback pixel2gl
    , glutCallbackEvent passiveMotionCallback pixel2gl
    , glutCallbackEvent idleCallback ($ IdleEvent)
    , glutCallbackEvent keyboardMouseCallback kbMouse
    ]

mouseMotionEvent :: UI -> Event (Float, Float)
mouseMotionEvent =
  eMapMaybe f
  where
    f (MouseMotionEvent x y) = Just (x, y)
    f _ = Nothing

glutKeyboardMouseEvent ::
  UI -> Event (Key, KeyState, Modifiers, Position)
glutKeyboardMouseEvent =
  eMapMaybe f
  where
    f (KeyboardMouseEvent k s m p) = Just (k, s, m, p)
    f _ = Nothing

glutIdleEvent :: UI -> Event ()
glutIdleEvent =
  eMapMaybe f
  where
    f IdleEvent = Just ()
    f _ = Nothing

draw :: Image -> IO ()
draw image = do
  clear [ ColorBuffer ]
  runImage image
  swapBuffers
  flush

run :: (UI -> (Event Image, SideEffect)) -> IO ()
run programDesc = do
  _ <- getArgsAndInitialize
  initialDisplayMode $~ (DoubleBuffered:)
  createWindow "test"
  displayCallback $= return ()
  (imageE, sideEffect) <- fmap programDesc createUI
  image <- runEvent imageE
  mapM_ draw (initialValues image)
  addHandler image draw
  executeSideEffect sideEffect
  mainLoop


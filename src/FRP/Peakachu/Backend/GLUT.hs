module FRP.Peakachu.Backend.GLUT (
  GlutToProgram(..), Image(..), ProgramToGlut(..), glut
  ) where

import FRP.Peakachu.Backend (Backend(..), Sink(..))

import Data.Monoid (Monoid(..))
import Graphics.UI.GLUT (
  ($=), ($~), get,
  ClearBuffer(..), Key(..), KeyState(..),
  Modifiers, Position(..), Size(..), Timeout,
  DisplayMode(..), initialDisplayMode, swapBuffers,
  createWindow, getArgsAndInitialize,
  displayCallback, idleCallback,
  keyboardMouseCallback,
  motionCallback, passiveMotionCallback,
  windowSize, addTimerCallback,
  clear, flush, mainLoop)

data Image = Image { runImage :: IO ()}

instance Monoid Image where
  mempty = Image $ return ()
  mappend (Image a) (Image b) = Image $ a >> b

data GlutToProgram a
  = IdleEvent
  | TimerEvent a
  | MouseMotionEvent Float Float
  | KeyboardMouseEvent Key KeyState Modifiers Position

data ProgramToGlut a
  = DrawImage Image
  | SetTimer Timeout a

glut :: Backend (ProgramToGlut a) (GlutToProgram a)
glut =
  Backend b
  where
    b handler = do
      _ <- getArgsAndInitialize
      initialDisplayMode $~ (DoubleBuffered:)
      createWindow "test"
      displayCallback $= return ()
      setCallbacks
      return Sink
        { sinkConsume = consume
        , sinkMainLoop = Just mainLoop
        , sinkQuitLoop = return ()
        }
      where
        consume (DrawImage image) = do
          clear [ ColorBuffer ]
          runImage image
          swapBuffers
          flush
        consume (SetTimer timeout tag) = do
          addTimerCallback timeout . handler . TimerEvent $ tag
        setCallbacks = do
          idleCallback $= Just (handler IdleEvent)
          keyboardMouseCallback $=
            Just (
            (fmap . fmap . fmap . fmap)
            handler KeyboardMouseEvent)
          motionCallback $= Just motion
          passiveMotionCallback $= Just motion
        motion (Position px py) = do
          Size sx sy <- get windowSize
          handler $ MouseMotionEvent (p2g sx px) (- p2g sy py)
        p2g sa pa = 2 * fromIntegral pa / fromIntegral sa - 1


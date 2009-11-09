{-# LANGUAGE TemplateHaskell #-}

module FRP.Peakachu.Backend.GLUT
  ( GlutToProgram(..), Image(..), ProgramToGlut(..), glut
  , gIdleEvent, gTimerEvent, gMouseMotionEvent
  , gKeyboardMouseEvent
  ) where

import Control.Concurrent.MVar.YC
import Data.ADT.Getters (mkADTGetters)
import FRP.Peakachu.Backend (Backend(..), Sink(..))

import Control.Concurrent.MVar
import Control.Concurrent (forkIO, threadDelay)
import Data.Monoid (Monoid(..))
import Graphics.UI.GLUT (
  GLfloat, ($=), ($~), get,
  ClearBuffer(..), Key(..), KeyState(..),
  Modifiers, Position(..), Size(..), Timeout,
  DisplayMode(..), initialDisplayMode, swapBuffers,
  createWindow, getArgsAndInitialize,
  displayCallback, idleCallback,
  keyboardMouseCallback,
  motionCallback, passiveMotionCallback,
  windowSize, {- addTimerCallback, -}
  clear, flush, mainLoop)

data Image = Image { runImage :: IO ()}

instance Monoid Image where
  mempty = Image $ return ()
  mappend (Image a) (Image b) = Image $ a >> b

data GlutToProgram a
  = IdleEvent
  | TimerEvent a
  | MouseMotionEvent GLfloat GLfloat
  | KeyboardMouseEvent Key KeyState Modifiers Position
$(mkADTGetters ''GlutToProgram)

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
      -- all the OpenGL drawing must be performed from the same thread
      -- that runs the GLUT event-loop.
      -- so instead of consuming when given input, we add it to the todo-list.
      -- the next time any GLUT event comes (should be immediate),
      -- we consume all the todo-list.
      -- without this mechanism the graphics flickers.
      todoVar <- newMVar []
      let
        consume (DrawImage image) = do
          clear [ ColorBuffer ]
          runImage image
          swapBuffers
          flush
        -- Ideally would be using addTimerCallback,
        -- but that doesn't seem to work properly.
        -- (sometimes the timer pops straight away)
        consume (SetTimer timeout tag) = do
          forkIO $ do
            threadDelay $ timeout*1000
            handler . TimerEvent $ tag
          return ()
          -- addTimerCallback timeout . handler count . TimerEvent $ tag
        preHandler event = do
          todo <- takeMVar todoVar
          putMVar todoVar []
          mapM_ consume . reverse $ todo
          handler event
        setCallbacks = do
          idleCallback $= Just (preHandler IdleEvent)
          keyboardMouseCallback $=
            Just (
            (fmap . fmap . fmap . fmap)
            preHandler KeyboardMouseEvent)
          motionCallback $= Just motion
          passiveMotionCallback $= Just motion
        motion (Position px py) = do
          Size sx sy <- get windowSize
          preHandler $ MouseMotionEvent (p2g sx px) (- p2g sy py)
        p2g sa pa = 2 * fromIntegral pa / fromIntegral sa - 1
      setCallbacks
      return Sink
        { sinkConsume = modifyMVarPure todoVar . (:)
        , sinkInit = handler $ MouseMotionEvent 0 0
        , sinkMainLoop = Just mainLoop
        , sinkQuitLoop = return () -- leaveMainLoop doesn't seem to work
        }


{-# LANGUAGE TemplateHaskell #-}

module FRP.Peakachu.Backend.GLUT
    ( GlutToProgram(..), Image(..), ProgramToGlut(..), glut
    , gIdleEvent, gTimerEvent, gMouseMotionEvent
    , gKeyboardMouseEvent
    ) where

import Control.Concurrent.MVar.YC (modifyMVarPure)
import Data.ADT.Getters (mkADTGetters)
import FRP.Peakachu.Backend (Backend(..))
import FRP.Peakachu.Backend.Internal
    (Sink(..), MainLoop(..), ParallelIO(..))

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Data.Monoid (Monoid(..))
import Graphics.UI.GLUT
    ( GLfloat, ($=), ($~), get
    , ClearBuffer(..), Key(..), KeyState(..)
    , Modifiers, Position(..), Size(..), Timeout
    , DisplayMode(..), initialDisplayMode, swapBuffers
    , createWindow, getArgsAndInitialize
    , displayCallback, idleCallback
    , keyboardMouseCallback
    , motionCallback, passiveMotionCallback
    , windowSize, addTimerCallback
    , clear, flush, mainLoop, leaveMainLoop
    )

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

glutConsume :: (GlutToProgram a -> IO ()) -> ProgramToGlut a -> IO ()
glutConsume _ (DrawImage image) = do
    clear [ ColorBuffer ]
    runImage image
    swapBuffers
    flush
glutConsume handler (SetTimer timeout tag) =
    -- there seems to be a bug with addTimerCallback.
    -- sometimes it calls you back straight away..
    -- but doing a work around with an io-thread seems
    -- to be very slow
    addTimerCallback timeout . handler . TimerEvent $ tag

setGlutCallbacks :: MVar [ProgramToGlut a] -> (GlutToProgram a -> IO ()) -> IO ()
setGlutCallbacks todoVar handler = do
    idleCallback $= Just (preHandler IdleEvent)
    keyboardMouseCallback $=
        Just (
        (fmap . fmap . fmap . fmap)
        preHandler KeyboardMouseEvent)
    motionCallback $= Just motion
    passiveMotionCallback $= Just motion
    where
        preHandler event = do
            todo <- takeMVar todoVar
            putMVar todoVar []
            mapM_ (glutConsume handler) . reverse $ todo
            handler event
        motion (Position px py) = do
            Size sx sy <- get windowSize
            preHandler $ MouseMotionEvent (p2g sx px) (- p2g sy py)    
        p2g sa pa = 2 * fromIntegral pa / fromIntegral sa - 1

glut :: Backend (ProgramToGlut a) (GlutToProgram a)
glut =
    Backend b
    where
        b handler = do
            _ <- getArgsAndInitialize
            initialDisplayMode $~ (DoubleBuffered:)
            _ <- createWindow "test"
            displayCallback $= return ()
            -- all the OpenGL drawing must be performed from the same thread
            -- that runs the GLUT event-loop.
            -- so instead of consuming when given input, we add it to the todo-list.
            -- the next time any GLUT event comes (should be immediate),
            -- we consume all the todo-list.
            -- without this mechanism the graphics flickers.
            todoVar <- newMVar []
            setGlutCallbacks todoVar handler
            return Sink
                { sinkConsume = modifyMVarPure todoVar . (:)
                , sinkMainLoop =
                    MainLoop
                    { mlInit = handler $ MouseMotionEvent 0 0
                    , mlQuit = leaveMainLoop
                    , mlRun = Just $ ParIO mainLoop
                    }
                }

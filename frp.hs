{-# OPTIONS -O2 -Wall #-}

import Control.Concurrent.MVar
import Control.Generator
import Control.Generator.Memo
import Control.Generator.ProducerT
import Control.Generator.Tools
import Control.Monad
import Control.Monad.Trans
import Data.Function
import Data.Monoid
import Graphics.UI.GLUT
import System.Time

-- core

type Time = ClockTime

data Event a = Event { runEvent :: Producer IO (Time, Maybe a) }

escanl :: (a -> b -> a) -> a -> Event b -> Event a
escanl step startVal src =
  Event . mmerge $ do
    startTime <- getClockTime
    return . imap post .
      iscanl process (startTime, startVal, True) $
      runEvent src
  where
    process (_, a, _) (time, b) =
      return $ case b of
        Nothing -> (time, a, False)
        Just x -> (time, step a x, True)
    post (time, _, False) = return (time, Nothing)
    post (time, a, True) = return (time, Just a)

efilter :: (a -> Bool) -> Event a -> Event a
efilter cond =
  Event . ifilter cond' . runEvent
  where
    cond' (_, Nothing) = return True
    cond' (_, Just x) = return $ cond x

emap :: (a -> b) -> Event a -> Event b
emap func =
  Event . imap func' . runEvent
  where
    func' (time, Nothing) = return (time, Nothing)
    func' (time, Just x) = return (time, Just (func x))
      

data Image = Image (IO ())

instance Monoid Image where
  mempty = Image $ return ()
  mappend (Image a) (Image b) = Image $ a >> b

-- tools

-- program

redSquare :: Image
redSquare =
  Image . renderPrimitive Quads $ do
    color $ Color3 1 0 (0 :: GLfloat)
    vertex $ Vertex3 0 0 (0 :: GLfloat)
    vertex $ Vertex3 0.5 0 (0 :: GLfloat)
    vertex $ Vertex3 0.5 0.5 (0 :: GLfloat)
    vertex $ Vertex3 0 0.5 (0 :: GLfloat)

greenSquare :: Image
greenSquare =
  Image . renderPrimitive Quads $ do
    color $ Color3 0 1 (0 :: GLfloat)
    vertex $ Vertex3 0 0 (0 :: GLfloat)
    vertex $ Vertex3 (-0.5) 0 (0 :: GLfloat)
    vertex $ Vertex3 (-0.5) 0.5 (0 :: GLfloat)
    vertex $ Vertex3 0 0.5 (0 :: GLfloat)

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
    liftIO . (idleCallback $=) . Just =<< processRest rest
  displayCallback $= return ()
  mainLoop

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  createWindow "test"
  kbPresses <- glKeyboardMouseEvents
  let
    post :: Int -> Image
    post 0 = redSquare
    post _ = greenSquare
  glutRun . emap post $ escanl (const . (1 -)) 0 kbPresses


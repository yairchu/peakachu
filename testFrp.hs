import FRP.Peakachu (emap, escanl)
import FRP.Peakachu.Backend.GLUT (Image(..), glKeyboardMouseEvents, glutRun)
import Graphics.UI.GLUT

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


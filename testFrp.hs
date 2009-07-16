import FRP.Peakachu
import FRP.Peakachu.Backend.GLUT
import Graphics.UI.GLUT

f :: (GLfloat, GLfloat) -> Image
f (x, y) =
  Image . renderPrimitive Quads $ do
    color $ Color3 1 0 (0 :: GLfloat)
    vertex $ Vertex3 x y (0 :: GLfloat)
    vertex $ Vertex3 0.5 0 (0 :: GLfloat)
    vertex $ Vertex3 0.5 0.5 (0 :: GLfloat)
    vertex $ Vertex3 0 0.5 (0 :: GLfloat)

main :: IO ()
main = glutRun $ emap f mouseMotionEvent


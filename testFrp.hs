import Control.Monad
import FRP.Peakachu
import FRP.Peakachu.Backend.GLUT
import Graphics.UI.GLUT

f :: (GLfloat, GLfloat) -> Image
f (x, y) =
  Image . renderPrimitive Quads $ do
    forM_ [1..10] $ \i -> do
      let f = 0.1 * fromIntegral i
      color $ Color3 f 0 0
      forM_ vs $ \(vx, vy) ->
        vertex $ Vertex2 (f * (x + 0.2 * vx)) (f * (y + 0.2 * vy))
  where
    vs = [((-1), (-1)), ((-1), 1), (1, 1), (1, (-1))]

main :: IO ()
main = glutRun $ emap f mouseMotionEvent


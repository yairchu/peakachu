import Control.Monad
import FRP.Peakachu
import FRP.Peakachu.Backend.GLUT
import Graphics.UI.GLUT

f :: (GLfloat, GLfloat) -> Image
f (x, y) =
  Image $ do
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    lighting $= Enabled
    --materialAmbient Front $= Color4 0 1 0 0.5
    materialDiffuse Front $= Color4 1 0 0 0.5
    light (Light 0) $= Enabled
    position (Light 0) $= Vertex4 1.5 1 (-2) 0
    --light (Light 1) $= Enabled
    --position (Light 1) $= Vertex4 (-1) (-1) 1 0
    color $ Color4 1 0 0 (0.5 :: GLfloat)
    renderPrimitive Quads $ do
      forM_ (zip vs (tail vs ++ [head vs])) $ \((ax, ay), (bx, by)) -> do
        normal $ Normal3 (abs ((ax + bx) / 2)) (abs ((ay + by) / 2)) (0 :: GLfloat)
        vertex $ Vertex4 (x + cs * ax) (y + cs * ay) 0 1
        vertex $ Vertex4 (2*rx + 0.25 * ax) (2*ry + 0.25 * ay) 0 2
        vertex $ Vertex4 (2*rx + 0.25 * bx) (2*ry + 0.25 * by) 0 2
        vertex $ Vertex4 (x + cs * bx) (y + cs * by) 0 1
      normal $ Normal3 0 0 (-1 :: GLfloat)
      forM_ vs $ \(vx, vy) ->
        vertex $ Vertex4 (x + cs * vx) (y + cs * vy) 0 1
  where
    vs = [((-1), (-1)), ((-1), 1), (1, 1), (1, (-1))]
    r v = (fromIntegral (round (4 * v + 0.5)) - 0.5) / 4
    rx = r x
    ry = r y
    cs = 0.05

main :: IO ()
main = glutRun $ emap f mouseMotionEvent


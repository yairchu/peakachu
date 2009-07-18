import Control.Monad
import Data.List
import FRP.Peakachu
import FRP.Peakachu.Backend.GLUT
import Graphics.UI.GLUT

faceNormal :: (Floating a, Ord a) => [[a]] -> [a]
faceNormal points =
  normalizeVec [a1*b2-a2*b1, a2*b0-a0*b2, a0*b1-a1*b0]
  where
    offset = head points
    base = map (zipWith (-) offset) (tail points)
    [[a0, a1, a2], [b0, b1, b2]] = base

normalizeVec vec
  | all (== 0) vec = vec
  | otherwise = map (/ norm) vec
  where
    norm = sqrt . sum $ map (^ 2) vec

f :: (GLfloat, GLfloat) -> Image
f (x, y) =
  Image $ do
    cursor $= None
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    lighting $= Enabled
    light (Light 0) $= Enabled
    position (Light 0) $= Vertex4 0 0 (-1) 0
    chessBoard
    drawCursor
  where
    chessBoard =
      forM_ [0..7] $ \bx ->
      forM_ [0..7] $ \by -> do
        let
          col = 0.2 + 0.1 * (fromIntegral ((bx + by) `mod` 2))
          r ba va = 0.125*((fromIntegral ba*2+va)-7)
        materialDiffuse Front $= Color4 col col col 1
        normal $ Normal3 0 0 (-1 :: GLfloat)
        renderPrimitive Quads .
          forM vs $ \(vx, vy) ->
            vertex $ Vertex4 (r bx vx) (r by vy) 0 1
    drawCursor =
      renderPrimitive Triangles .
      forM_ (zip vs (tail vs ++ [head vs])) $
      \((ax, ay), (bx, by)) -> do
        let
          r v = (fromIntegral (round (4 * v + 0.5)) - 0.5) / 4
          rx = r x
          ry = r y
          points =
            [[0.9*x, 0.9*y, 0.9]
            ,[rx + 0.125*ax, ry + 0.125*ay, 1]
            ,[rx + 0.125*bx, ry + 0.125*by, 1]
            ]
          norml = faceNormal points
          [nx, ny, nz]
            | last norml < 0 = norml
            | otherwise = map negate norml
        normal $ Normal3 nx ny nz
        materialDiffuse Front $= Color4 1 1 0 1
        forM_ (take 1 points) $ \[px, py, pz] ->
          vertex $ Vertex4 px py 0 pz
        materialDiffuse Front $= Color4 1 1 0 0
        forM_ (tail points) $ \[px, py, pz] ->
          vertex $ Vertex4 px py 0 pz
    vs = [((-1), (-1)), ((-1), 1), (1, 1), (1, (-1))]

main :: IO ()
main = glutRun $ emap f mouseMotionEvent


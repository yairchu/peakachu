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

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King

type BoardPos = (Integer, Integer)

data Piece = Piece {
  pieceType :: PieceType,
  piecePos :: BoardPos
}

data Board = Board {
  boardPieces :: [Piece]
}

data PiecePix = PiecePix {
  pixBody :: [[(GLfloat, GLfloat)]],
  pixOutline :: [(GLfloat, GLfloat)]
}

piecePix :: PieceType -> PiecePix
piecePix Rook = PiecePix {
  pixBody =
    [[(-1, -1), (1, -1), (1, t-1), (-1, t-1)]
    ,[(t-1, t-1), (1-t, t-1), (1-t, 1-t), (t-1, 1-t)]
    ,[(-1, 1), (1, 1), (1, 1-t), (-1, 1-t)]
    ],
  pixOutline =
    [(1, t-1), (1, -1), (-1, -1), (-1, t-1)
    ,(t-1, t-1), (t-1, 1-t)
    ,(-1, 1-t), (-1, 1), (1, 1), (1, 1-t)
    ,(1-t, 1-t), (1-t, t-1)
    ]
  }
  where
    t = 0.35
piecePix Knight = PiecePix {
  pixBody =
    [[(-1, 0), (1, 1), (1, 0)]
    ,[(0, 0), (1, 0), (1, -1), (-1, -1)]
    ],
  pixOutline =
    [(-1, 0), (1, 1), (1, -1), (-1, -1), (0, 0)]
  }
piecePix Bishop = PiecePix {
  pixBody = [outline],
  pixOutline = outline
  }
  where
    outline = [(0, 1), (1, -1), (-1, -1)]

pieceAt :: Board -> BoardPos -> Maybe Piece
pieceAt board pos =
  case filter ((== pos) . piecePos) (boardPieces board) of
    [] -> Nothing
    (x : _) -> Just x

draw :: (Board, (GLfloat, GLfloat)) -> Image
draw (board, (cx, cy)) =
  Image $ do
    cursor $= None
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    lighting $= Enabled
    light (Light 0) $= Enabled
    position (Light 0) $= Vertex4 0 0 (-1) 0
    cullFace $= Nothing
    drawBoard
    forM_ (boardPieces board) drawPiece
    cullFace $= Just Front
    drawCursor
  where
    screenPos pa = (fromIntegral pa - 3.5) / 4
    boardPos ca = round (4 * ca + 3.5)
    bcx = boardPos cx
    bcy = boardPos cy
    headingUp = normal $ Normal3 0 0 (-1 :: GLfloat)
    drawPiece piece = do
      let
        pix = piecePix (pieceType piece)
        (px, py) = piecePos piece
        sx = screenPos px
        sy = screenPos py
      materialDiffuse Front $= Color4 1 1 1 (1 :: GLfloat)
      headingUp
      forM (pixBody pix) $ \poly -> do
        let
          polyType
            | 3 == length poly = Triangles
            | otherwise = Quads
        renderPrimitive polyType .
          forM poly $ \(vx, vy) ->
            vertex $ Vertex4
              (sx + 0.125*pieceSize*vx)
              (sy + 0.125*pieceSize*vy) 0 1
    pieceSize = 0.9
    drawBoard =
      forM_ [0..7] $ \bx ->
      forM_ [0..7] $ \by -> do
        let
          col = 0.2 + 0.1 * (fromIntegral ((bx + by) `mod` 2))
          r ba va = 0.125*((fromIntegral ba*2+va)-7)
        materialDiffuse Front $= Color4 col col col 1
        headingUp
        renderPrimitive Quads .
          forM square $ \(vx, vy) ->
            vertex $ Vertex4 (r bx vx) (r by vy) 0 1
    drawCursor =
      renderPrimitive Triangles .
      forM_ (zip curPix (tail curPix ++ [head curPix])) $
      \((ax, ay), (bx, by)) -> do
        let
          rx = screenPos bcx
          ry = screenPos bcy
          points =
            [[0.9*cx, 0.9*cy, 0.9]
            ,[rx + 0.125*ax, ry + 0.125*ay, 1]
            ,[rx + 0.125*bx, ry + 0.125*by, 1]
            ]
          norml = faceNormal points
          [nx, ny, nz]
            | last norml < 0 = norml
            | otherwise = map negate norml
        normal $ Normal3 nx ny nz
        materialDiffuse Front $=
          case pieceUnderCursor of
            Nothing -> Color4 1 1 0 1
            otherwise -> Color4 0 1 0 1
        forM_ (take 1 points) $ \[px, py, pz] ->
          vertex $ Vertex4 px py 0 pz
        materialDiffuse Front $=
          case pieceUnderCursor of
            Nothing -> Color4 1 1 0 0
            otherwise -> Color4 0 1 0 0.5
        forM_ (tail points) $ \[px, py, pz] ->
          vertex $ Vertex4 px py 0 pz
    pieceUnderCursor = pieceAt board (bcx, bcy)
    curPix =
      case pieceUnderCursor of
        Nothing -> square
        Just p -> map t . pixOutline . piecePix $ pieceType p
      where
        t (x, y) = (pieceSize*x, pieceSize*y)
    square = [((-1), (-1)), ((-1), 1), (1, 1), (1, (-1))]

chessStart :: Board
chessStart = Board [
  Piece Rook (0, 0), Piece Knight (1, 0), Piece Bishop (2, 0)
  ]

main :: IO ()
main =
  glutRun . emap draw $
  ezip' (ereturn chessStart) mouseMotionEvent


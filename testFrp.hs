import Chess
import ChessFont

import Control.Monad (forM)
import Data.Foldable (forM_)
import Data.List
import Data.Monoid
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

pieceAt :: Board -> BoardPos -> Maybe Piece
pieceAt board pos =
  case filter ((== pos) . piecePos) (boardPieces board) of
    [] -> Nothing
    (x : _) -> Just x

screen2board :: DrawPos -> BoardPos
screen2board (cx, cy) =
  (r cx, r cy)
  where
    r ca = round (4 * ca + 3.5)

board2screen :: BoardPos -> DrawPos
board2screen (bx, by) =
  (r bx, r by)
  where
    r ba = (fromIntegral ba - 3.5) / 4

type Selection = (BoardPos, Maybe BoardPos)

draw :: (Board, (Selection, DrawPos)) -> Image
draw (board, ((dragSrc, dragDst), (cx, cy))) =
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
    drawCursor dragSrc
    forM_ dragDst drawCursor
  where
    headingUp = normal $ Normal3 0 0 (-1 :: GLfloat)
    drawPiece piece = do
      let
        pix = piecePix (pieceType piece)
        (px, py) = piecePos piece
        (sx, sy) = board2screen (px, py)
        col :: Color4 GLfloat
        col
          | pieceSide piece == White = Color4 1 1 1 1
          | otherwise = Color4 0 0.1 1 1
      materialDiffuse Front $= col
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
    drawCursor boardPos = do
      cullFace $= Just Back
      drawCursor' boardPos
      cullFace $= Just Front
      drawCursor' boardPos
    drawCursor' boardPos =
      renderPrimitive Triangles .
      forM_ curPix $ \part ->
      forM_ (zip part (tail part ++ [head part])) $
      \((ax, ay), (bx, by)) -> do
        let
          (rx, ry) = board2screen boardPos
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
    pieceUnderCursor = pieceAt board dragSrc
    curPix =
      case pieceUnderCursor of
        Nothing -> [square]
        Just p -> map (map t) . pixOutline . piecePix $ pieceType p
      where
        t (x, y) = (pieceSize*x, pieceSize*y)
    square = [((-1), (-1)), ((-1), 1), (1, 1), (1, (-1))]

keyState :: Key -> Event KeyState
keyState key =
  mappend (ereturn Up) .
  fmap m $
  efilter f glKeyboardMouseEvent
  where
    m (_, state, _, _) = state
    f (k, _, _, _) = k == key

delayEvent :: Integral i => i -> Event a -> Event a
delayEvent count =
  fmap last .
  edrop count .
  escanl step []
  where
    step xs x = genericTake count $ x : xs

main :: IO ()
main =
  glutRun . fmap draw . ezip' board $ ezip' selection mouseMotionEvent
  where
    board = escanl doMove chessStart moves
    doMove board (src, dst) =
      Board . map m $ boardPieces board
      where
        m piece
          | piecePos piece /= src = piece
          | otherwise = piece { piecePos = dst }
    selection =
      fmap snd .
      edrop 1 .
      escanl drag (Up, undefined) $
      ezip' (keyState (MouseButton LeftButton)) mouseMotionEvent
    drag (Down, (x, _)) (Down, c) = (Down, (x, Just (screen2board c)))
    drag _ (s, c) =
      (s, (spos, dst))
      where
        spos = screen2board c
        dst
          | s == Up = Nothing
          | otherwise = Just spos
    moves =
      fmap (moveProc . fst) $
      efilter moveFilter $
      ezip' (delayEvent 1 selection) selection
    moveProc (a, Just b) = (a, b)
    moveProc _ = undefined
    moveFilter ((_, Just _), (_, Nothing)) = True
    moveFilter _ = False


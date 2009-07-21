module Chess where

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King

type BoardPos = (Integer, Integer)

data PieceSide = Black | White
  deriving Eq

data Piece = Piece {
  pieceSide :: PieceSide,
  pieceType :: PieceType,
  piecePos :: BoardPos
}

data Board = Board {
  boardPieces :: [Piece]
}

chessStart :: Board
chessStart = Board (
  concat (zipWith headRowItems [0..7] headRowTypes)
  ++ [Piece side Pawn (x, y) |
  x <- [0..7], (side, y) <- [(White, 1), (Black, 6)]])
  where
    headRowTypes = [Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook]
    headRowItems x t = [Piece White t (x, 0), Piece Black t (x, 7)]

addPos :: BoardPos -> BoardPos -> BoardPos
addPos (xa, ya) (xb, yb) = (xa+xb, ya+yb)

rays :: PieceType -> [[BoardPos]]
rays Knight = do
  a <- [1, -1]
  b <- [2, -2]
  [[(a, b)], [(b, a)]]
rays Bishop = do
  dx <- [1, -1]
  dy <- [1, -1]
  return $ iterate (addPos (dx, dy)) (dx, dy)
rays Rook = do
  (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]
  return $ iterate (addPos (dx, dy)) (dx, dy)
rays Queen = rays Bishop ++ rays Rook
rays King = map (take 1) $ rays Queen
rays Pawn = []

possibleMoves :: Piece -> Board -> [(BoardPos, Board)]
possibleMoves piece board = do
  relRay <- rays (pieceType piece)
  dst <- takeWhile notBlocked $ map (addPos (piecePos piece)) relRay
  return (dst, board)
  where
    notBlocked (x, y) =
      0 <= x && x < 8 && 0 <= y && y < 8


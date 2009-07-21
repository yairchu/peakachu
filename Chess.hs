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


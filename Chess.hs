module Chess (
  PieceType(..), BoardPos, PieceSide(..), Piece(..), Board(..),
  chessStart, pieceAt, possibleMoves
  ) where

import Control.Monad (guard)
import Data.Foldable (Foldable, all, any)
import Prelude hiding (all, any, null)

data PieceType =
  Pawn | Knight | Bishop | Rook | Queen | King
  deriving Eq

type BoardPos = (Integer, Integer)

data PieceSide = Black | White
  deriving Eq

data Piece = Piece {
  pieceSide :: PieceSide,
  pieceType :: PieceType,
  piecePos :: BoardPos
}

data Board = Board {
  boardPieces :: [Piece],
  boardLastMove :: Maybe (Piece, BoardPos)
}

chessStart :: Board
chessStart =
  Board {
    boardPieces =
      concat (zipWith headRowItems [0..7] headRowTypes) ++
      [Piece side Pawn (x, y) |
      x <- [0..7], (side, y) <- [(White, 1), (Black, 6)]],
    boardLastMove = Nothing
  }
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

pieceAt :: Board -> BoardPos -> Maybe Piece
pieceAt board pos =
  case filter ((== pos) . piecePos) (boardPieces board) of
    [] -> Nothing
    (x : _) -> Just x

takeUntilIncluding :: (a -> Bool) -> [a] -> [a]
takeUntilIncluding _ [] = []
takeUntilIncluding func (x : xs)
  | func x = [x]
  | otherwise = x : takeUntilIncluding func xs

null :: Foldable t => t a -> Bool
null = all (const False)

possibleMoves :: Board -> Piece -> [(BoardPos, Board)]
possibleMoves board piece =
  simpleMoves ++ otherMoves (pieceType piece)
  where
    simpleMoves = do
      relRay <- rays (pieceType piece)
      dst <-
        takeUntilIncluding (not . null . pieceAt board) .
        takeWhile notBlocked $
        map (addPos src) relRay
      return $ simpleMove dst
    src = piecePos piece
    simpleMove dst =
      (dst, newBoard)
      where
        newBoard =
          Board {
            boardPieces =
              newPieceState :
              filter (not . (`elem` [src, dst]) . piecePos)
              (boardPieces board),
            boardLastMove = Just (newPieceState, src)
          }
        newPieceState = piece { piecePos = dst }
    inBoard (x, y) = 0 <= x && x < 8 && 0 <= y && y < 8
    isOtherSide = (/= pieceSide piece) . pieceSide
    notBlocked pos =
      inBoard pos &&
      all isOtherSide (pieceAt board pos)
    otherMoves Pawn =
      (enPassant (boardLastMove board) ++) $
      map simpleMove .
      filter inBoard $
      moveForward ++
      filter (any isOtherSide . pieceAt board)
        [(sx-1, sy+forward), (sx+1, sy+forward)] ++
      do
        guard $ sy == pawnStartRow
        guard . not $ null moveForward
        guard . null $ pieceAt board sprintDst
        return sprintDst
      where
        moveForward = filter (null . pieceAt board) [(sx, sy+forward)]
        sprintDst = (sx, sy+forward*2)
        enPassant Nothing = []
        enPassant (Just (lpiece, m@(mx, my)))
          | my /= sy = []
          | abs (mx-sx) /= 1 = []
          | pieceType lpiece /= Pawn = []
          | pieceSide lpiece == pieceSide piece = []
          | not (null (pieceAt board dst)) = []
          | otherwise =
            [(dst, Board {
              boardPieces =
                newPieceState :
                filter (not . (`elem` [src, dst, m]) . piecePos)
                (boardPieces board),
              boardLastMove = Just (newPieceState, src)
            })]
          where
            dst = (mx, sy+forward)
            newPieceState = piece { piecePos = dst }
    otherMoves _ = []
    (forward, pawnStartRow)
      | pieceSide piece == White = (1, 1)
      | otherwise = (-1, 6)
    (sx, sy) = src


import Control.Applicative
import Control.Category ((.))
import Control.FilterCategory (arrC)
import Data.Monoid
import Data.Ord (Ordering)
import FRP.Peakachu.Program
import FRP.Peakachu.Program.Cont
import Prelude hiding ((.))

import FRP.Peakachu
import FRP.Peakachu.Backend.StdIO

-- | Monadic binary search.
--
-- Gets a function for asking whether an index is:
-- * Correct (EQ)
-- * Too small (LT). The guess is smaller than the solution
-- * Too large (GT)
binarySearchM :: (Monad m, Integral i) => (i -> m Ordering) -> i -> i -> m i
binarySearchM check low high
  | low == high = return low
  | otherwise = do
    let mid = (low + high) `div` 2
    comp <- check mid
    case comp of
      EQ -> return mid
      LT -> binarySearchM check (mid+1) high
      GT -> binarySearchM check low (mid-1)

startMsg :: String
startMsg = "Please think of an item in [1..100]"

askMsg :: Show a => a -> String
askMsg x =
  "Is " ++ show x ++ " the number? (EQ=yes, LT=too low, GT=too high)"

endMsg :: Show a => a -> String
endMsg x = "The number is " ++ show x ++ "!"

guessGameM :: IO ()
guessGameM = do
  putStrLn startMsg
  answer <- binarySearchM ((>> readLn) . putStrLn . askMsg) 1 100
  putStrLn . endMsg $ answer

data BinSchOp i = Ask i | Answer i

binarySearchP :: Integral i => i -> i -> Program Ordering (BinSchOp i)
binarySearchP low high =
  toProgram $
    binarySearchM ((>> inputP) . outputP . Ask) low high
    >>= outputP . Answer

guessGameP :: Program String String
guessGameP =
  withAppendProgram1 (mappend (return startMsg)) $
  arrC out . binarySearchP 1 100 . arrC read
  where
    out (Ask i) = askMsg i
    out (Answer i) = endMsg i


module FRP.Peakachu.Backend.File (
  readFileE, writeFileE
  ) where

import FRP.Peakachu.Internal (Event, SideEffect(..), ejoin)

import Control.Monad (join)
import Data.Function (fix)
import System.IO (IOMode(ReadMode), openFile, hClose, hGetChar)
import System.IO.Error (try, isEOFError)

maybeIO :: (IOError -> Bool) -> IO a -> IO (Maybe a)
maybeIO isExpected =
  join . fmap f . try
  where
    f (Right x) = return $ Just x
    f (Left err)
      | isExpected err = return Nothing
      | otherwise = ioError err

-- Lazy IO forbidden because imho it is horrible
strictReadFile :: FilePath -> IO String
strictReadFile filename = do
  file <- openFile filename ReadMode
  contents <- fix $ \rest -> do
    mc <- maybeIO isEOFError $ hGetChar file
    case mc of
      Nothing -> return ""
      Just c -> fmap (c :) rest
  hClose file
  return contents

readFileE :: Event FilePath -> Event String
readFileE = ejoin . fmap strictReadFile

writeFileE :: Event (FilePath, String) -> SideEffect
writeFileE = SideEffect . fmap (uncurry writeFile)


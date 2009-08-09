module FRP.Peakachu.Backend.File (
  readFileE, writeFileE
  ) where

import FRP.Peakachu (Event, EffectfulFunc, SideEffect)
import FRP.Peakachu.Backend.IO (mkEffectfulFunc, mkSideEffect)

import Control.Monad (join)
import Control.Monad.Trans (lift)
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

readFileE :: IO (EffectfulFunc FilePath String a)
readFileE = mkEffectfulFunc (lift . strictReadFile)

writeFileE :: Event (FilePath, String) -> SideEffect
writeFileE = mkSideEffect (uncurry writeFile)


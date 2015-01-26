{-# LANGUAGE TemplateHaskell #-}

module FRP.Peakachu.Backend.File
    ( FileToProgram(..), ProgramToFile(..), fileB
    , gFileData, gFileError
    ) where

import Data.ADT.Getters (mkADTGetters)
import FRP.Peakachu.Backend (Backend(..))
import FRP.Peakachu.Backend.Internal (Sink(..))

import Control.Monad (join)
import Data.Function (fix)
import Data.Monoid (Monoid(..))
import System.IO (IOMode(ReadMode), openFile, hClose, hGetChar)
import System.IO.Error (isEOFError)
import Control.Exception (try)

data FileToProgram a
    = FileData String a
    | FileError a
$(mkADTGetters ''FileToProgram)

data ProgramToFile a
    = ReadFile FilePath a
    | WriteFile FilePath String a

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

fileB :: Backend (ProgramToFile a) (FileToProgram a)
fileB =
    Backend f
    where
        f handler =
            return mempty { sinkConsume = consume }
            where
                consume (ReadFile filename tag) =
                    strictReadFile filename >>=
                    handler . (`FileData` tag)
                consume (WriteFile filename contents _) =
                    writeFile filename contents


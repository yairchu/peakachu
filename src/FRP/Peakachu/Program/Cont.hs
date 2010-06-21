module FRP.Peakachu.Program.Cont
    ( toProgram, inputP, outputP, breakP
    ) where

import FRP.Peakachu.Program (Program(..), withAppendProgram1, emptyP)

import Control.Monad.Cont (Cont(..))
import Data.Monoid (mappend)

toProgram :: Cont (Program i o) a -> Program i o
toProgram = (`runCont` const emptyP)

modCont :: (r -> r) -> Cont r ()
modCont = Cont . (. ($ ()))

outputP :: o -> Cont (Program i o) ()
outputP = modCont . withAppendProgram1 . mappend . return

inputP :: Cont (Program i o) i
inputP = Cont (Program [] . Just)

breakP :: Cont (Program i o) ()
breakP = Cont . return $ emptyP


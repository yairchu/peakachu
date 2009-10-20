{-# LANGUAGE TemplateHaskell #-}

module FRP.Peakachu.Backend.GLUT.Getters where

import Control.FilterCategory (FilterCategory(..), mapMaybeC)
import Data.ADT.Getters (mkADTGetterCats)

import Graphics.UI.GLUT (Key(..), KeyState(..))

$(mkADTGetterCats ''Key)
$(mkADTGetterCats ''KeyState)


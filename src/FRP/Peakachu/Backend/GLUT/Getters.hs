{-# LANGUAGE TemplateHaskell #-}

module FRP.Peakachu.Backend.GLUT.Getters where

import Data.ADT.Getters (mkADTGetters)

import Graphics.UI.GLUT (Key(..), KeyState(..))

$(mkADTGetters ''Key)
$(mkADTGetters ''KeyState)


-- | ADT getter functions for GLUT data types.
--
-- Useful for filtering GLUT events in the Maybe monad.

{-# LANGUAGE TemplateHaskell #-}

module FRP.Peakachu.Backend.GLUT.Getters
    ( gChar, gMouseButton, gSpecialKey, gDown, gUp
    ) where

import Data.ADT.Getters (mkADTGetters)

import Graphics.UI.GLUT (Key(..), KeyState(..))

$(mkADTGetters ''Key)
$(mkADTGetters ''KeyState)


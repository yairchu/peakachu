{-# LANGUAGE TemplateHaskell #-}

module Data.Cons
  ( Cons(..)
  ) where

import Control.Applicative.Define
import Data.Newtype

import Control.Applicative
import Control.Compose
import Data.Monoid

data Cons m a = Cons
  { headC :: a
  , tailC :: m a
  }
$(mkWithNewtypeFuncs [0..2] ''Id)
$(mkApplicative
  [d| instance Applicable f => Applicable (Cons f) |]
  [["withId", "lift"], ["lift"]])
instance (Applicative f, Monoid a) => Monoid (Cons f a) where
  mempty  = pure mempty
  mappend = liftA2 mappend


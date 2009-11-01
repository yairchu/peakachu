{-# LANGUAGE TemplateHaskell #-}

module Data.InfiniteStream.Item 
  ( InfiniteStreamItem(..)
  ) where

import Control.Applicative.Define
import Data.Newtype

import Control.Applicative
import Control.Compose
import Data.Monoid

data InfiniteStreamItem m a = InfStrIt
  { headIS :: a
  , tailIS :: m a
  }
$(mkWithNewtypeFuncs [0..2] ''Id)
$(mkApplicative
  [d| instance Applicable f => Applicable (InfiniteStreamItem f) |]
  [["withId", "lift"], ["lift"]])
instance (Applicative f, Monoid a) => Monoid (InfiniteStreamItem f a) where
  mempty  = pure mempty
  mappend = liftA2 mappend


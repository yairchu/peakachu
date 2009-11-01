{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Lift.Overlap () where

import Control.Lift

import Control.Applicative

instance MakeApplicative f => Functor f where
  fmap  = uplift1 applifter

instance MakeApplicative f => Applicative f where
  pure  = uplift0 applifter
  (<*>) = uplift2 applifter ($)


{-# LANGUAGE Rank2Types, TypeOperators #-}

module Control.Lift 
  ( Lift(..), MakeApplicative(..)
  , Applicable(..), mkApp, mkApplicative, monadToApplicative
  , uplift0, uplift1, uplift2
  , (~.), lifto
  , appLift, biLift, monadLift
  ) where

import Control.Applicative (Applicative(..), liftA2)
import Control.Compose ((:.)(..), Id(..), inId, inId2, inO, inO2)
import Control.Monad (liftM, liftM2)
import Data.Bijection (Bijection(..))
import Language.Haskell.TH.Syntax hiding (Lift(..))

data Lift f g = Lift
  { lift0 :: forall a. f a -> g a
  , lift1 :: forall a b. (f a -> f b) -> g a -> g b
  , lift2 :: forall a b c. (f a -> f b -> f c) -> g a -> g b -> g c
  }

uplift0 :: Lift Id f -> a -> f a
uplift0 l = lift0 l . Id

uplift1 :: Lift Id f -> (a -> b) -> f a -> f b
uplift1 l = lift1 l . inId

uplift2 :: Lift Id f -> (a -> b -> c) -> f a -> f b -> f c
uplift2 l = lift2 l . inId2

class MakeApplicative f where
  applifter :: Lift Id f

class Applicable f where
  applicableDummy :: f ()
  applicableDummy = undefined

mkApplicative :: [Dec] -> [Dec]
mkApplicative decs =
  mkApp cxt insType
  where
    [InstanceD cxt (AppT _ insType) _] = decs

instanceDef :: Cxt -> String -> Type -> [Dec] -> Dec
instanceDef context cls =
  InstanceD context . AppT (ConT (mkName cls))

clause :: Exp -> Clause
clause x = Clause [] (NormalB x) []

mkApp :: Cxt -> Type -> [Dec]
mkApp context insType =
  [ instanceDef context "Functor" insType
    [ FunD (mkName "fmap") [clause (lift 1)]
    ]
  , instanceDef context "Applicative" insType
    [ FunD (mkName "pure") [clause (lift 0)]
    , FunD (mkName "<*>") [clause (AppE (lift 2) (varE "$"))]
    ]
  ]
  where
    varE = VarE . mkName
    lift :: Int -> Exp
    lift i =
      AppE (varE ("uplift" ++ show i))
      . varE $ "applifter"

monadToApplicative :: [Dec] -> [Dec]
monadToApplicative decs =
  [ instanceDef context "MakeApplicative" insType
    [ FunD (mkName "applifter") [(clause . VarE . mkName) "monadLift"]
    ]
  ] ++ mkApp context insType
  where
    [InstanceD context (AppT _ insType) _] = decs

infixr 9 ~.

-- Lift is almost a Category
-- except it is of kind :: (* -> *) -> (* -> *) -> *
-- and not :: * -> * -> *
(~.) :: Lift b c -> Lift a b -> Lift a c
left ~. right =
  Lift
  { lift0 = lift0 left . lift0 right
  , lift1 = lift1 left . lift1 right
  , lift2 = lift2 left . lift2 right
  }

withId :: (Id a -> Id b) -> a -> b
withId = (unId .) . (. Id)

withId2 :: (Id a -> Id b -> Id c) -> a -> b -> c
withId2 f a b = unId $ f (Id a) (Id b)

monadLift :: Monad m => Lift Id m
monadLift =
  Lift
  { lift0 = return . unId
  , lift1 = liftM  . withId
  , lift2 = liftM2 . withId2
  }

appLift :: Applicative f => Lift Id f
appLift =
  Lift
  { lift0 = pure   . unId
  , lift1 = fmap   . withId
  , lift2 = liftA2 . withId2
  }

lifto :: Lift Id f -> Lift g (f :. g)
lifto orig =
  Lift
  { lift0 = O    . lift0 orig . Id
  , lift1 = inO  . lift1 orig . inId
  , lift2 = inO2 . lift2 orig . inId2
  }

biLift :: (forall a. Bijection (->) (f a) (g a)) -> Lift f g
biLift bij =
  Lift l0 l1 l2
  where
    l0 = biTo bij
    l1 = (biTo bij .) . (. biFrom bij)
    l2 f a b = biTo bij $ f (biFrom bij a) (biFrom bij b)

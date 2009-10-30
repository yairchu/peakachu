module Control.Applicative.Define
  ( Applicable, mkApplicative
  ) where

import Control.Applicative ((<$>))
import Language.Haskell.TH.Syntax

class Applicable f where
  applicableDummy :: a -> f a
  applicableDummy = undefined

mkApplicative :: Q [Dec] -> [[String]] -> Q [Dec]
mkApplicative mInstDec paths = do
  [InstanceD context (AppT _ insType) []] <- mInstDec
  let
    typeName = get insType
    get (ConT r) = r
    get (AppT r _) = get r
    get _ = undefined
  info <- reify typeName
  return $ mkApplicative' info insType context paths

mkApplicative' :: Info -> Type -> [Type] -> [[String]] -> [Dec]
mkApplicative' info instanceType context [path] =
  [ instanceDef "Functor"
    [ FunD (mkName "fmap")
      [ Clause [] (NormalB (liftFunc 1)) []
      ]
    ]
  , instanceDef "Applicative"
    [ FunD (mkName "pure")
      [ Clause [] (NormalB (liftFunc 0)) []
      ]
    , FunD (mkName "<*>")
      [ Clause [] (NormalB (InfixE (Just (liftFunc 2)) apply (Just apply))) []
      ]
    ]
  ]
  where
    apply = VarE . mkName $ "$"
    instanceDef typeclass =
      InstanceD (map (procCxt typeclass) context ++ typeContext) . AppT (ConT (mkName typeclass)) $ instanceType
    procCxt typeclass orig@(AppT (ConT someclass) x)
      | nameBase someclass == "Applicable" =
        AppT (ConT (mkName typeclass)) x
      | otherwise = orig
    procCxt typeclass x = x
    liftFunc numArgs =
      foldr1 compose $
      VarE . mkName . (++ show numArgs) <$> ("in" ++ nameBase typeName) : path
    compose l r = InfixE (Just l) (VarE (mkName ("."))) (Just r)
    TyConI typedef = info
    (typeContext, typeName, _, constructor) =
      case typedef of
        NewtypeD cxt tn tv cn _ -> (cxt, tn, tv, cn)
        DataD cxt tn tv [cn] _ -> (cxt, tn, tv, cn)
        _ -> undefined


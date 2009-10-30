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
mkApplicative' info instanceType context paths =
  [ instanceDef "Functor"
    [ FunD (mkName "fmap") [normClause (1::Int)]
    ]
  , instanceDef "Applicative"
    [ FunD (mkName "pure") [normClause (0::Int)]
    , FunD (mkName "<*>") [clause [] (`AppE` apply) (2::Int)]
    ]
  ]
  where
    argName i j = mkName $ "x_" ++ show i ++ "_" ++ show j
    apply = VarE . mkName $ "$"
    instanceDef typeclass =
      InstanceD (map (procCxt typeclass) context ++ typeContext) . AppT (ConT (mkName typeclass)) $ instanceType
    procCxt typeclass orig@(AppT (ConT someclass) x)
      | nameBase someclass == "Applicable" =
        AppT (ConT (mkName typeclass)) x
      | otherwise = orig
    procCxt _ x = x
    normClause = clause [VarP nameFunc] (`AppE` VarE nameFunc)
    clause prefArgs proc order =
      Clause
      (prefArgs ++ map mkPat [0 .. order-1])
      (NormalB altLiftFunc)
      []
      where
        altLiftFunc =
          foldl AppE (ConE consName) . zipWith go [0::Int ..] $ paths
        go consIdx path =
          foldl AppE (proc (liftFunc path order))
          $ VarE . (`argName` consIdx) <$> [0 .. order-1]
    mkPat i =
      ConP consName $ VarP . argName i <$> [0 .. consNumArgs-1]
    nameFunc = mkName "func"
    liftFunc [] _ = VarE . mkName $ "id"
    liftFunc path numArgs =
      foldr1 compose $
      VarE . mkName . (++ show numArgs) <$> path
    compose l r = InfixE (Just l) (VarE (mkName ("."))) (Just r)
    TyConI typedef = info
    (typeContext, constructor) =
      case typedef of
        NewtypeD cxt _ _ cn _ -> (cxt, cn)
        DataD cxt _ _ [cn] _ -> (cxt, cn)
        _ -> undefined
    (consName, consNumArgs) = digCons constructor
    digCons (NormalC n a) = (n, length a)
    digCons (RecC n a) = (n, length a)
    digCons (InfixC _ n _) = (n, 2)
    digCons (ForallC _ _ x) = digCons x


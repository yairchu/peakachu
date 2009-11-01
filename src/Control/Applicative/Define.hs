module Control.Applicative.Define
  ( Applicable, mkApplicative, mkApplicativeAndPointed
  ) where

import Control.Applicative ((<$>))
import Language.Haskell.TH.Syntax

class Applicable f where
  applicableDummy :: a -> f a
  applicableDummy = undefined

mkApplicative :: Q [Dec] -> [[String]] -> Q [Dec]
mkApplicative = mkAppNPoi False

-- | Pointed not also generated with Applicative.
-- So as to not force a dependency of category-extras on users
mkApplicativeAndPointed :: Q [Dec] -> [[String]] -> Q [Dec]
mkApplicativeAndPointed = mkAppNPoi True

mkAppNPoi :: Bool -> Q [Dec] -> [[String]] -> Q [Dec]
mkAppNPoi andPointed mInstDec paths = do
  [InstanceD context (AppT _ insType) []] <- mInstDec
  let
    typeName = get insType
    get (ConT r) = r
    get (AppT r _) = get r
    get _ = undefined
  info <- reify typeName
  return $ mkApp andPointed info insType context paths

mkApp :: Bool -> Info -> Type -> [Type] -> [[String]] -> [Dec]
mkApp andPointed info instanceType context paths =
  [ instanceDef "Functor"
    [ FunD (mkName "fmap") [normClause (1::Int) "fmap"]
    ]
  , instanceDef "Applicative"
    [ FunD (mkName "pure") [normClause (0::Int) "pure"]
    , FunD (mkName "<*>") [clause [] (`AppE` apply) (2::Int) "liftA2"]
    ]
  ] ++ filter (const andPointed)
  [ instanceDef "Pointed"
    [ FunD (mkName "point") [normClause (1::Int) "point"]
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
    clause prefArgs proc order appLift =
      Clause
      (prefArgs ++ map mkPat [0 .. order-1])
      (NormalB altLiftFunc)
      []
      where
        altLiftFunc =
          foldl AppE (ConE consName) . zipWith go [0::Int ..] $ paths
        go consIdx path =
          foldl AppE (proc (liftFunc path order appLift))
          $ VarE . (`argName` consIdx) <$> [0 .. order-1]
    mkPat i =
      ConP consName $ VarP . argName i <$> [0 .. consNumArgs-1]
    nameFunc = mkName "func"
    liftFunc [] _ _ = VarE . mkName $ "id"
    liftFunc path numArgs appLift =
      foldr1 compose $
      VarE . mkName . modPathItem <$> path
      where
        modPathItem "lift" = appLift
        modPathItem x = x ++ show numArgs
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


module Control.Applicative.Define
  ( mkApplicative
  ) where

import Control.Applicative ((<$>))
import Language.Haskell.TH.Syntax

mkApplicative :: Name -> [[String]] -> Q [Dec]
mkApplicative typeName paths = do
  info <- reify typeName
  return $ mkApplicative' info paths

mkApplicative' :: Info -> [[String]] -> [Dec]
mkApplicative' info [path] =
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
      InstanceD context . AppT (ConT (mkName typeclass)) $ instanceType
    liftFunc numArgs =
      foldr1 compose $
      VarE . mkName . (++ show numArgs) <$> path
    compose l r = InfixE (Just l) (VarE (mkName ("."))) (Just r)
    instanceType = foldl AppT (ConT typeName) $ VarT <$> init typeVars
    TyConI typedef = info
    (context, typeName, typeVars, constructor) =
      case typedef of
        NewtypeD cxt tn tv cn _ -> (cxt, tn, tv, cn)
        DataD cxt tn tv [cn] _ -> (cxt, tn, tv, cn)
        _ -> undefined


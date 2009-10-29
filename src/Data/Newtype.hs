module Data.Newtype where

import Control.Applicative
import Language.Haskell.TH.Syntax

nameAddSuf :: String -> Name -> Name
nameAddSuf suf name = mkName (nameBase name ++ suf)

typeAddSuf :: String -> Type -> Type
typeAddSuf suf (ForallT names cxt typ) =
  ForallT
    (nameAddSuf suf <$> names)
    (typeAddSuf suf <$> cxt)
    (typeAddSuf suf typ)
typeAddSuf suf (VarT name) = VarT (nameAddSuf suf name)
typeAddSuf suf (AppT left right) =
  AppT (typeAddSuf suf left) (typeAddSuf suf right)
typeAddSuf _ x = x

mkNewtypeInFuncs :: Int -> Name -> Q [Dec]
mkNewtypeInFuncs inMax typeName = do
  info <- reify typeName
  return $ [1..inMax] >>= mkNewTypeInFunc info

mkNewTypeInFunc :: Info -> Int -> [Dec]
mkNewTypeInFunc info funcIdx =
  [ SigD inFuncName
    . ForallT
      ( nameAddSuf <$> typeSuffixes <*> typeVars )
      ( typeAddSuf <$> typeSuffixes <*> context )
    . AppT 
      ( AppT ArrowT
        ( AppT
          ( AppT ArrowT (typeAddSuf "0" inType) )
          ( typeAddSuf "1" inType )
        )
      )
    . AppT
      ( AppT ArrowT (typeAddSuf "0" fullType) )
    . typeAddSuf "1" $ fullType
  , FunD inFuncName
    [ Clause [VarP fName, ConP consName [VarP xName]]
      ( NormalB
      . AppE (ConE consName)
      . AppE (VarE fName)
      . VarE $ xName
      ) []
    ]
  ]
  where
    TyConI newtypeDef = info
    NewtypeD context typeName typeVars constructor _ = newtypeDef
    (consName, inType) =
      case constructor of
        NormalC c [(_, i)] -> (c, i)
        RecC c [(_, _, i)] -> (c, i)
        _ -> undefined
    fName = mkName "f"
    xName = mkName "x"
    inFuncName = mkName $ "in" ++ nameBase typeName
    typeSuffixes = ["0", "1"]
    fullType = foldl AppT (ConT typeName) (map VarT typeVars)


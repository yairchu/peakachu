module Data.Newtype
  ( mkNewtypeInFuncs
  ) where

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

mkNewtypeInFuncs :: [Int] -> Name -> Q [Dec]
mkNewtypeInFuncs idx typeName = do
  info <- reify typeName
  return $ idx >>= mkNewTypeInFunc info

mkNewTypeInFunc :: Info -> Int -> [Dec]
mkNewTypeInFunc info funcIdx =
  [ SigD resName
    . ForallT
      ( nameAddSuf <$> typeSuffixes <*> typeVars )
      ( typeAddSuf <$> typeSuffixes <*> context )
    . AppT (AppT ArrowT (mkFuncType inType))
    $ mkFuncType fullType
  , FunD resName
    [ Clause (VarP fName : (ConP consName . return . VarP <$> xNames))
      ( NormalB
      . AppE (ConE consName)
      . foldl AppE (VarE fName)
      $ VarE <$> xNames
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
    xNames
      | 1 == funcIdx = [mkName "x"]
      | otherwise = mkName . ('x' :) . show <$> [0 .. funcIdx - 1]
    resName = mkName $ "in" ++ nameBase typeName ++ nameSuf
    nameSuf
      | 1 == funcIdx = ""
      | otherwise = show funcIdx
    typeSuffixes = show <$> [0 .. funcIdx]
    fullType = foldl AppT (ConT typeName) (map VarT typeVars)
    mkFuncType base =
      foldr AppT (typeAddSuf (last typeSuffixes) base)
      $ AppT ArrowT . (`typeAddSuf` base) <$> init typeSuffixes


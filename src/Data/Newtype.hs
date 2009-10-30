module Data.Newtype
  ( mkInNewtypeFuncs, mkWithNewtypeFuncs
  ) where

import Control.Applicative ((<$>), (<*>))
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

data NewtypeFunc = In | With

mkInNewtypeFuncs :: [Int] -> Name -> Q [Dec]
mkInNewtypeFuncs idx typeName = do
  info <- reify typeName
  return $ idx >>= mkNewTypeFunc info In

mkWithNewtypeFuncs :: [Int] -> Name -> Q [Dec]
mkWithNewtypeFuncs idx typeName = do
  info <- reify typeName
  return $ idx >>= mkNewTypeFunc info With

mkNewTypeFunc :: Info -> NewtypeFunc -> Int -> [Dec]
mkNewTypeFunc info whatFunc funcIdx =
  [ SigD resName
    . ForallT
      ( nameAddSuf <$> typeSuffixes <*> typeVars )
      ( typeAddSuf <$> typeSuffixes <*> context )
    . AppT (AppT ArrowT (mkFuncType inputType))
    $ mkFuncType outputType
  , FunD resName [clause]
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
    xNames = mkName . ('x' :) . show <$> [0 .. funcIdx - 1]
    resName = mkName $ prefix ++ nameBase typeName ++ show funcIdx
    typeSuffixes = show <$> [0 .. funcIdx]
    fullType = foldl AppT (ConT typeName) (map VarT typeVars)
    mkFuncType base =
      foldr AppT (typeAddSuf (last typeSuffixes) base)
      $ AppT ArrowT . (`typeAddSuf` base) <$> init typeSuffixes
    withResName = mkName "res"
    (prefix, inputType, outputType, clause) =
      case whatFunc of
        In ->
          ( "in", inType, fullType
          , Clause (VarP fName : (ConP consName . return . VarP <$> xNames))
            ( NormalB
            . AppE (ConE consName)
            . foldl AppE (VarE fName)
            $ VarE <$> xNames
            ) []
          )
        With ->
          ( "with", fullType, inType
          , Clause (VarP <$> fName : xNames)
            (NormalB (VarE withResName))
            [ ValD (ConP consName [VarP withResName])
              ( NormalB
              . foldl AppE (VarE fName)
              $ AppE (ConE consName) . VarE <$> xNames
              ) []
            ]
          )

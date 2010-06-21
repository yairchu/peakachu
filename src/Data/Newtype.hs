{-# LANGUAGE CPP #-}

-- | In/with newtype functions generation with Template Haskell.
--
-- Example:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import Control.Applicative (Applicative(..), ZipList(..))
-- > import Data.Newtype (mkWithNewTypeFuncs)
-- >
-- > $(mkWithNewtypeFuncs [2] ''ZipList)
-- >
-- > > withZipList2 (<*>) [(+3), (*3)] [6, 7]
-- > [9, 21]

module Data.Newtype
    ( mkInNewtypeFuncs, mkWithNewtypeFuncs
    ) where

import Control.Applicative ((<$>), (<*>))
import Language.Haskell.TH.Syntax

nameAddSuf :: String -> Name -> Name
nameAddSuf suf name = mkName (nameBase name ++ suf)

#if !(MIN_VERSION_template_haskell(2,4,0))
type TyVarBndr = Name
type Pred = Type
#endif

tyVarBndrAddSuf :: String -> TyVarBndr -> TyVarBndr
#if MIN_VERSION_template_haskell(2,4,0)
tyVarBndrAddSuf suf (PlainTV name) = PlainTV (nameAddSuf suf name)
tyVarBndrAddSuf suf (KindedTV name kind) = KindedTV (nameAddSuf suf name) kind
#else
tyVarBndrAddSuf = nameAddSuf
#endif

predAddSuf :: String -> Pred -> Pred
#if MIN_VERSION_template_haskell(2,4,0)
predAddSuf suf (ClassP name types) = ClassP name (map (typeAddSuf suf) types)
predAddSuf suf (EqualP a b) = EqualP (typeAddSuf suf a) (typeAddSuf suf b)
#else
predAddSuf = typeAddSuf
#endif

tyVarBndrName :: TyVarBndr -> Name
#if MIN_VERSION_template_haskell(2,4,0)
tyVarBndrName (PlainTV name) = name
tyVarBndrName (KindedTV name _) = name
#else
tyVarBndrName = id
#endif

typeAddSuf :: String -> Type -> Type
typeAddSuf suf (ForallT names cxt typ) =
    ForallT
        (tyVarBndrAddSuf suf <$> names)
        (predAddSuf suf <$> cxt)
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
            ( tyVarBndrAddSuf <$> typeSuffixes <*> typeVars )
            ( predAddSuf <$> typeSuffixes <*> context )
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
        fullType = foldl AppT (ConT typeName) (map (VarT . tyVarBndrName) typeVars)
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

{-# LANGUAGE CPP #-}

-- | ADT getters generation with Template Haskell
--
-- Example:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > data Blah a = NoBlah | YesBlah a | ManyBlah a Int
-- > $(mkADTGetters ''Blah)
--
-- Generates
--
-- > gNoBlah :: Blah a -> Maybe ()
-- > gYesBlah :: Blah a -> Maybe a
-- > gManyBlah :: Blah a -> Maybe (a, Int)
--
-- Where
--
-- > gYesBlah (YesBlah a) = Just a
-- > gYesBlah _ = Nothing
--
-- etc.

module Data.ADT.Getters
    ( mkADTGetters
    ) where

import Language.Haskell.TH.Syntax

mkADTGetters :: Name -> Q [Dec]
mkADTGetters typeName = do
    TyConI (DataD _ _ typeVars constructors _) <- reify typeName
    return $ constructors >>= mkADTGetterFunc typeName typeVars

#if !(MIN_VERSION_template_haskell(2,4,0))
type TyVarBndr = Name
#endif

tyVarBndrName :: TyVarBndr -> Name
#if MIN_VERSION_template_haskell(2,4,0)
tyVarBndrName (PlainTV name) = name
tyVarBndrName (KindedTV name _) = name
#else
tyVarBndrName = id
#endif

mkADTGetterFunc :: Name -> [TyVarBndr] -> Con -> [Dec]
mkADTGetterFunc typeName typeVars constructor =
    [ SigD resName
        . ForallT typeVars []
        . AppT (AppT ArrowT (foldl AppT (ConT typeName) (map (VarT . tyVarBndrName) typeVars)))
        . AppT (ConT (mkName "Maybe"))
        $ case containedTypes of
        [] -> TupleT 0
        [x] -> x
        xs -> foldl AppT (TupleT (length xs)) xs
    , FunD resName
        [ Clause [ConP name (map VarP varNames)] clauseJust []
        , Clause [WildP] clauseNothing []
        ]
    ]
    where
        NormalC name params = constructor
        containedTypes = map snd params
        resName = mkName $ 'g' : nameBase name
        varNames = map (mkName . ('x' :) . show) [0 .. length params - 1]
        clauseJust =
            NormalB . AppE (ConE (mkName "Just"))
            $ case varNames of
            [] -> TupE []
            [x] -> VarE x
            xs -> TupE (map VarE xs)
        clauseNothing = NormalB . ConE . mkName $ "Nothing"


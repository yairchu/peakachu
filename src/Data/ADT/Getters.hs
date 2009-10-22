-- | ADT getters
--
-- Example:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > data Blah a = NoBlah | YesBlah a | ManyBlah a Int
-- > $(mkADTGetterFuncs ''Blah)
--
-- Generates
--
-- > gNoBlah :: Blah a -> Maybe ()
-- > gYesBlah :: Blah a -> Maybe a
-- > gManyBlah :: Blah a -> Maybe (a, Int)

module Data.ADT.Getters
  ( mkADTGetterFuncs
  , mkADTGetterCats
  ) where

import Language.Haskell.TH.Syntax

mkADTGetterFuncs :: Name -> Q [Dec]
mkADTGetterFuncs typeName = do
  TyConI (DataD _ _ typeVars constructors _) <- reify typeName
  return $ constructors >>= mkADTGetterFunc typeName typeVars

mkADTGetterFunc :: Name -> [Name] -> Con -> [Dec]
mkADTGetterFunc typeName typeVars constructor =
  [ SigD resName
    . ForallT typeVars []
    . AppT (AppT ArrowT (foldl AppT (ConT typeName) (map VarT typeVars)))
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

mkADTGetterCats :: Name -> Q [Dec]
mkADTGetterCats typeName = do
  TyConI (DataD _ _ typeVars constructors _) <- reify typeName
  funcs <- mkADTGetterFuncs typeName
  return $
    funcs ++
    (constructors >>= mkADTGetterCat typeName typeVars)

mkADTGetterCat :: Name -> [Name] -> Con -> [Dec]
mkADTGetterCat typeName typeVars constructor =
  [ SigD resName
    . ForallT (catName : typeVars)
      [ AppT filterCategory (VarT catName)
      , AppT functor func
      ]
    . AppT func
    $ case containedTypes of
    [] -> TupleT 0
    [x] -> x
    xs -> foldl AppT (TupleT (length xs)) xs
  , FunD resName
    [ Clause [] clause []
    ]
  ]
  where
    inputType = foldl AppT (ConT typeName) (map VarT typeVars)
    filterCategory = ConT . mkName $ "FilterCategory"
    functor = ConT . mkName $ "Functor"
    func = AppT (VarT catName) inputType
    catName = mkName "cat"
    NormalC name params = constructor
    containedTypes = map snd params
    genName = mkName . (++ nameBase name)
    resName = genName "c"
    clause =
      NormalB
      . AppE (VarE (mkName "mapMaybeC"))
      . VarE . genName $ "g"


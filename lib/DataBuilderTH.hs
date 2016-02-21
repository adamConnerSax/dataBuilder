{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module DataBuilderTH where

import Language.Haskell.TH
import qualified Data.Map as M

import Data.Char (toLower)

type TypeName = String
type FieldName = String
type ConName = String
data Metadata = Metadata { typeName::TypeName, conName::Maybe ConName, fieldName::Maybe FieldName} 

typeOnlyMD::TypeName->Metadata
typeOnlyMD tn = Metadata tn Nothing Nothing

data MDWrapped f a = MDWrapped { hasDefault::Bool, metadata::Metadata, value::f a }

class Buildable f where
  -- inject and apply are exactly Applicative, inject=pure and apply=(<*>). 
  bInject::a -> f a
  bApply::f (a->b) -> f a -> f b
  bSum::[MDWrapped f a]->f a

class Builder f a where
  buildM::Buildable f=>Metadata->Maybe a-> f a

type ConId = ConName

unsupported :: String -> Q a
unsupported msg = fail ("Unsupported type: " ++ show msg)

conNameAndTypes::Con->Q (Name,[Type],Maybe [Name])
conNameAndTypes (NormalC n ts) = return (n, snd <$> ts,Nothing)
conNameAndTypes (RecC n vts) =
  let thrd (_,_,x) = x
      first (x,_,_) = x in return (n,thrd <$> vts, Just (first <$> vts))
conNameAndTypes c = unsupported ("constructor type in conNameAndTypes (" ++ show c ++ ")")

getCons (DataD _ _ _ c _) = return c
getCons (NewtypeD _ _ _ c _) = return [c]
getCons (TySynD _ _ (ConT n)) = lookupType n
getCons (SigD _ (ConT n)) = lookupType n
getCons x = unsupported ("type in getCons " ++ show x)

typeNameE :: Type -> Q Exp
typeNameE (ConT n) = return (sToE $ nameBase n)
typeNameE (VarT n) = return (sToE $ nameBase n)
typeNameE (TupleT n) = return (sToE $ show n ++ "-tuple")
typeNameE x = unsupported ("type in typeName " ++ show x)

lookupType :: Name -> Q [Con]
lookupType n = do
  conInfo <- reify n
  case conInfo of
    TyConI dec -> getCons dec
    _          -> unsupported ("reify of " ++ show n ++ " returned other than TyConI.")

deriveBuild::Name -> Name ->Q [Dec]
deriveBuild builderName typeName = do
  [d|instance Builder $(conT builderName) $(conT typeName) where
       buildM md Nothing  = $(handleNothingL typeName) md
       buildM md (Just x) = $(handleJustL typeName) md x|]

handleNothingL::Name->Q Exp
handleNothingL n = do
  mdN <- newName "md"
  let patsQ = [return $ VarP mdN]
      mdE = VarE mdN
  lamE patsQ (handleNothing n mdE)

handleNothing::Name->Exp->Q Exp
handleNothing n mdE = do
  sumFE <- [e|bSum|]
  blankBuildersMap <- buildAllBlankBuilders n mdE
  -- [e|bSum|] `appE` (listE . snd . unzip . M.toList $ blankBuilderMap)
  let summedE = sumFE `AppE` (ListE . snd . unzip . M.toList $ blankBuildersMap)
  return $ summedE

handleJustL::Name->Q Exp
handleJustL n = do
  mdN <- newName "md"
  aN <- newName "a"
  let patsQ = return <$> [VarP mdN, VarP aN]
      mdE = VarE mdN        
      aE = VarE aN
  lamE patsQ (handleJust n mdE aE) 

handleJust::Name->Exp->Exp->Q Exp
handleJust n mdE varAE = do
  cons <- lookupType n
  blankBuildersMap <- buildAllBlankBuilders n mdE
  let matchBuilder con = buildCaseMatch (nameBase n) mdE con blankBuildersMap 
  matches <- mapM matchBuilder cons
  return $ CaseE varAE matches

buildAllBlankBuilders::Name->Exp->Q (M.Map ConId Exp) 
buildAllBlankBuilders n mdE = do
  cons <- lookupType n
  let first (x,_,_) = x
  cnames <- (fmap first) <$> (mapM conNameAndTypes cons)
  bldrs <- mapM (buildBlankBuilder (nameBase n) mdE) cons
  let cids = map nameBase cnames
  return $ M.fromList (zip cids bldrs)

sToE::String->Exp
sToE = LitE . StringL

buildCaseMatch::TypeName->Exp->Con->M.Map ConId Exp->Q Match
buildCaseMatch typeN mdE c builderMap = do
  nothingE <- [e|Nothing|]
  justE <- [e|Just|] 
  bldME <- [e|buildM|]
  applyE  <- [e|bApply|]
  injectE <- [e|bInject|]
  sumFE <- [e|bSum|]
  (n,tl,mFNs) <- conNameAndTypes c
  ns <- mapM (\(ConT x)->newName $ toLower <$> (nameBase x)) tl
  tnEs <- mapM typeNameE tl
  metaCE <- [e|Metadata|]
  toMetaCE <- [e|typeOnlyMD|]
  fieldNameE <- [e|fieldName|]
  mdWrappedE <- [e|MDWrapped True|]
  let conMetaE = metaCE `AppE` (sToE typeN) `AppE` (justE `AppE` sToE (nameBase n)) `AppE` (fieldNameE `AppE` mdE) 
  let pats = VarP <$> ns
      vars = VarE <$> ns
      mdEs = case mFNs of
        Nothing -> map (\tnE -> toMetaCE `AppE` tnE) tnEs
        Just fnames -> map (\(tnE,fn) -> metaCE `AppE` tnE `AppE` nothingE `AppE` (justE `AppE` (sToE $ nameBase fn))) (zip tnEs fnames)
      bldrs = map (\(mdE,vE)->bldME `AppE` mdE `AppE` (justE `AppE` vE)) (zip mdEs vars)
      conFE = injectE `AppE` (ConE n)
      bldr = foldl (\e1 e2 -> applyE `AppE` e1 `AppE` e2) conFE bldrs
      mdwE = mdWrappedE `AppE` conMetaE `AppE` bldr
      newMap = M.insert (nameBase n) mdwE builderMap
      summedE = sumFE `AppE` (ListE . snd . unzip . M.toList $ newMap)
  return $ Match (ConP n pats) (NormalB summedE) []

buildBlankBuilder::TypeName->Exp->Con->Q Exp
buildBlankBuilder typeN mdE c = do
  nothingE <- [e|Nothing|]
  justE <- [e|Just|]
  bldME <- [e|buildM|]
  applyE  <- [e|bApply|]
  injectE <- [e|bInject|]
  metaCE <- [e|Metadata|]
  toMetaCE <- [e|typeOnlyMD|]
  fieldNameE <- [e|fieldName|]
  mdWrappedE <- [e|MDWrapped False|]
  (n,tl,mFNs) <- conNameAndTypes c
  tnEs <- mapM typeNameE tl
  -- create Metadata for field types
  let mdEs = case mFNs of
        Nothing -> map (\tnE -> toMetaCE `AppE` tnE) tnEs
        Just fnames -> map (\(tnE,fn) -> metaCE `AppE` tnE `AppE` nothingE `AppE` (justE `AppE` (sToE $ nameBase fn))) (zip tnEs fnames)
      bldrs = map (\mdE->bldME `AppE` mdE `AppE` nothingE) mdEs 
  let conFE = injectE `AppE` (ConE n)
      foldBldrs = foldl (\e1 e2 -> applyE `AppE` e1 `AppE` e2) conFE bldrs
      conMetaE = metaCE `AppE` (sToE typeN) `AppE` (justE `AppE` (sToE $ nameBase n)) `AppE` (fieldNameE `AppE` mdE) 
  return $ mdWrappedE `AppE` conMetaE `AppE` foldBldrs




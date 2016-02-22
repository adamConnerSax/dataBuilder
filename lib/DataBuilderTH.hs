{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module DataBuilderTH
       (
         TypeName
       , FieldName
       , ConName
       , Metadata(typeName,conName,fieldName)
       , MDWrapped(hasDefault,metadata,value)
       , typeOnlyMD
       , Buildable(..)
       , Builder(..)
       , deriveBuilder
       )where

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
--InfixC and ForallC.  We could extract name and types but not sure we'd know how to apply them??
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

deriveBuilder::Name -> Name ->Q [Dec]
deriveBuilder builderName typeName = do
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
  [e|bSum $(listE . snd . unzip . M.toList $ blankBuildersMap)|]

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

buildAllBlankBuilders::Name->Exp->Q (M.Map ConId ExpQ) 
buildAllBlankBuilders n mdE = do
  cons <- lookupType n
  let first (x,_,_) = x
  cnames <- (fmap first) <$> (mapM conNameAndTypes cons)
  let bldrs = map (buildBlankBuilder (nameBase n) mdE) cons
  let cids = map nameBase cnames
  return $ M.fromList (zip cids bldrs)

sToE::String->Exp
sToE = LitE . StringL

sToE'::String->ExpQ
sToE' = litE . stringL

zipE::[ExpQ]->[ExpQ]->[ExpQ]
zipE e1s e2s = (tupE . (\(x,y)->[x,y])) <$> (zip e1s e2s)


builderPre::TypeName->Exp->Con->Q (Name,[Type],ExpQ,ExpQ,[ExpQ])
builderPre typeN mdE c = do
  (n,tl,mFNs) <- conNameAndTypes c
  let tnEs = map typeNameE tl
      conMetaE = [e|Metadata $(sToE' typeN) (Just $(sToE' $ nameBase n)) (fieldName $(return mdE))|] 
      conFE = [e|bInject $(conE n)|]
      mdEs = case mFNs of
        Nothing -> map (appE [e|typeOnlyMD|]) tnEs
        Just fnames -> map (appE [e|\(tn,fn) -> Metadata tn Nothing (Just fn)|]) (zipE tnEs (map (sToE' . nameBase) fnames))
  return (n,tl,conMetaE,conFE,mdEs)

buildBlankBuilder::TypeName->Exp->Con->Q Exp
buildBlankBuilder typeN mdE c = do
  (n,tl,conMetaE,conFE,mdEs) <- builderPre typeN mdE c
  let bldrs = map (appE [e|flip buildM Nothing|]) mdEs
      bldr = foldl (\e1 e2 -> [e|bApply|] `appE` e1 `appE` e2) conFE bldrs --This has to fold over Exps, otherwise bApply has multiple types during the fold
  [e|MDWrapped False $conMetaE $bldr|]

buildCaseMatch::TypeName->Exp->Con->M.Map ConId ExpQ->Q Match
buildCaseMatch typeN mdE c builderMap = do
  (n,tl,conMetaE,conFE,mdEs) <- builderPre typeN mdE c
  ns <- mapM (\(ConT x)->newName $ toLower <$> (nameBase x)) tl
  let bldrs = map (appE [e|\(md,v)-> buildM md (Just v)|]) (zipE mdEs (varE <$> ns))
      bldr = foldl (\e1 e2 -> [e|bApply|] `appE` e1 `appE` e2) conFE bldrs --This has to fold over Exps, otherwise bApply has multiple types during the fold
      mdwE = [e|MDWrapped True $conMetaE $bldr|]
      newMap = M.insert (nameBase n) mdwE builderMap
      summedE = [e|bSum $(listE . snd . unzip . M.toList $ newMap)|]
  match (conP n (varP <$> ns)) (normalB summedE) []




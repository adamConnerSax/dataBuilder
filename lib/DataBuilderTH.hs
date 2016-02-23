{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module DataBuilderTH
       (
         TypeName
       , FieldName
       , ConName
       , Metadata(typeName,conName,fieldName)
       , typeOnlyMD
       , HasMetadata(..)
       , MDWrapped(hasDefault,metadataHolder,value)
       , mdwConName
       , Buildable(..)
       , Builder(..)
       , deriveBuilder
       ) where

import Language.Haskell.TH
import qualified Data.Map as M

import Data.Char (toLower)
import Data.Maybe (fromJust)

type TypeName = String
type FieldName = String
type ConName = String
data Metadata = Metadata { typeName::TypeName, conName::Maybe ConName, fieldName::Maybe FieldName} 

typeOnlyMD::TypeName->Metadata
typeOnlyMD tn = Metadata tn Nothing Nothing

class HasMetadata a where
  getMetadata::a->Metadata
  setMetadata::Metadata->a->a

instance HasMetadata Metadata where
  getMetadata = id
  setMetadata x _ = x

data MDWrapped f g a = MDWrapped { hasDefault::Bool, metadataHolder::g, value::f a }

instance HasMetadata g=>HasMetadata (MDWrapped f g a) where
  getMetadata = getMetadata . metadataHolder 
  setMetadata md (MDWrapped hd mdh v) = let mdh' = setMetadata md mdh in MDWrapped hd mdh' v

mdwConName::HasMetadata g=>MDWrapped f g a -> Maybe ConName
mdwConName x = conName (getMetadata x) 

mdwHasConName::HasMetadata g=>MDWrapped f g a->Bool
mdwHasConName mdw = maybe False (const True) (mdwConName mdw)

buildersAllHaveConNames::HasMetadata g=>[MDWrapped f g a]->Bool
buildersAllHaveConNames bes = null (filter (not . mdwHasConName) bes) 


class Buildable f g | f->g where
  -- inject and apply are exactly Applicative, inject=pure and apply=(<*>). 
  bInject::a -> f a
  bApply::f (a->b) -> f a -> f b
  bFail::String->f a -- if there's a graceful way to handle errors...
  bSum::[MDWrapped f g a]->f a -- this only gets called if you have > 1 and all have constructor names in Metadata

class Builder f g a where
  buildM::(HasMetadata g,Buildable f g)=>g->Maybe a-> f a

internalSum::(HasMetadata g, Buildable f g)=>[MDWrapped f g a]->f a
internalSum mdws = case (length mdws) of
  0 -> bFail "Internal error in DataBuilder.  No Constructors in Sum!"
  1 -> value (head mdws)
  _ -> if (buildersAllHaveConNames mdws) then bSum mdws else bFail "Sum type for command encountered but constructor name(s) are missing."

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

typeShow :: Type -> Maybe String
typeShow (ConT n) = Just $ toLower <$> nameBase n
typeShow (VarT n) = Just $ toLower <$> nameBase n
typeShow (TupleT n) = Just $ "tuple" ++ show n
typeShow (AppT t1 t2) = do
  ts1 <- typeShow t1
  ts2 <- typeShow t2
  return $ (toLower <$> ts1) ++ ts2
typeShow _ = Nothing

typeNameE :: Type -> Q Exp
typeNameE x = maybe (unsupported ("type in typeName " ++ show x)) sToE' (typeShow x) 

lookupType :: Name -> Q [Con]
lookupType n = do
  conInfo <- reify n
  case conInfo of
    TyConI dec -> getCons dec
    _          -> unsupported ("reify of " ++ show n ++ " returned other than TyConI.")

deriveBuilder::Name -> Name -> Name -> Q [Dec]
deriveBuilder builderName metadataHolderName typeName = do
  [d|instance Builder $(conT builderName) $(conT metadataHolderName) $(conT typeName) where
       buildM mdh Nothing  = $(handleNothingL typeName) mdh
       buildM mdh (Just x) = $(handleJustL typeName) mdh x|]

handleNothingL::Name->Q Exp
handleNothingL n = do
  mdhN <- newName "mdh"
  let patsQ = [return $ VarP mdhN]
      mdhE = VarE mdhN
  lamE patsQ (handleNothing n mdhE)

handleNothing::Name->Exp->Q Exp
handleNothing n mdhE = do
  sumFE <- [e|bSum|]
  blankBuildersMap <- buildAllBlankBuilders n mdhE
  [e|internalSum $(listE . snd . unzip . M.toList $ blankBuildersMap)|]

handleJustL::Name->Q Exp
handleJustL n = do
  mdhN <- newName "mdh"
  aN <- newName "a"
  let patsQ = return <$> [VarP mdhN, VarP aN]
      mdhE = VarE mdhN        
      aE = VarE aN
  lamE patsQ (handleJust n mdhE aE) 

handleJust::Name->Exp->Exp->Q Exp
handleJust n mdhE varAE = do
  cons <- lookupType n
  blankBuildersMap <- buildAllBlankBuilders n mdhE
  let matchBuilder con = buildCaseMatch (nameBase n) mdhE con blankBuildersMap 
  matches <- mapM matchBuilder cons
  return $ CaseE varAE matches

buildAllBlankBuilders::Name->Exp->Q (M.Map ConId ExpQ) 
buildAllBlankBuilders n mdhE = do
  cons <- lookupType n
  let first (x,_,_) = x
  cnames <- (fmap first) <$> (mapM conNameAndTypes cons)
  let bldrs = map (buildBlankBuilder (nameBase n) mdhE) cons
      cids = map nameBase cnames
  return $ M.fromList (zip cids bldrs)

sToE::String->Exp
sToE = LitE . StringL

sToE'::String->ExpQ
sToE' = litE . stringL

zipE::[ExpQ]->[ExpQ]->[ExpQ]
zipE e1s e2s = (tupE . (\(x,y)->[x,y])) <$> (zip e1s e2s)

builderPre::TypeName->Exp->Con->Q (Name,[Type],ExpQ,ExpQ,[ExpQ])
builderPre typeN mdhE c = do
  (n,tl,mFNs) <- conNameAndTypes c
  let tnEs = map typeNameE tl
      conMetaHE = [e|setMetadata (Metadata $(sToE' typeN) (Just $(sToE' $ nameBase n)) ((fieldName . getMetadata) $(return mdhE))) $(return mdhE)|] 
      conFE = [e|bInject $(conE n)|]
      mdhEs = case mFNs of
        Nothing -> map (appE [e|\x->setMetadata (typeOnlyMD x) $(return mdhE)|]) tnEs
        Just fnames -> map (appE [e|\(tn,fn) -> setMetadata (Metadata tn Nothing (Just fn)) $(return mdhE)|]) (zipE tnEs (map (sToE' . nameBase) fnames))
  return (n,tl,conMetaHE,conFE,mdhEs)

buildBlankBuilder::TypeName->Exp->Con->Q Exp
buildBlankBuilder typeN mdhE c = do
  (n,tl,conMetaE,conFE,mdhEs) <- builderPre typeN mdhE c
  let bldrs = map (appE [e|flip buildM Nothing|]) mdhEs
      bldr = foldl (\e1 e2 -> [e|bApply|] `appE` e1 `appE` e2) conFE bldrs --This has to fold over Exps, otherwise bApply has multiple types during the fold
  [e|MDWrapped False $conMetaE $bldr|]


buildCaseMatch::TypeName->Exp->Con->M.Map ConId ExpQ->Q Match
buildCaseMatch typeN mdhE c builderMap = do
  (n,tl,conMetaHE,conFE,mdhEs) <- builderPre typeN mdhE c
  ns <- mapM (\ty->newName $ fromJust (typeShow ty)) tl
  let bldrs = map (appE [e|\(mdh,v)-> buildM mdh (Just v)|]) (zipE mdhEs (varE <$> ns))
      bldr = foldl (\e1 e2 -> [e|bApply|] `appE` e1 `appE` e2) conFE bldrs --This has to fold over Exps, otherwise bApply has multiple types during the fold
      mdwE = [e|MDWrapped True $conMetaHE $bldr|]
      newMap = M.insert (nameBase n) mdwE builderMap
      summedE = [e|internalSum $(listE . snd . unzip . M.toList $ newMap)|]
  match (conP n (varP <$> ns)) (normalB summedE) []




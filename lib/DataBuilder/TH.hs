{-# LANGUAGE TemplateHaskell #-}

module DataBuilder.TH
       (
         deriveBuilder
       ) where

import Language.Haskell.TH
import qualified Data.Map as M

import Data.Char (toLower)
import Data.Maybe (fromJust)
import DataBuilder.InternalTypes

type ConId = ConName

unsupported :: String -> Q a
unsupported msg = fail ("Unsupported type: " ++ show msg)

conNameAndTypes::Con->Q (Name,[Type],Maybe [Name])
conNameAndTypes (NormalC n ts) = return (n, snd <$> ts,Nothing)
conNameAndTypes (RecC n vts) =
  let thrd (_,_,x) = x
      first (x,_,_) = x in return (n,thrd <$> vts, Just (first <$> vts))
conNameAndTypes (InfixC (_,t) n (_,t')) = return (n, [t,t'], Nothing)
--ForallC.  We could extract name and types but not sure we'd know how to apply them??
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
        Nothing -> map (appE [e|\x->setTypeName $(return mdhE) x|]) tnEs
        Just fnames -> map (appE [e|\(tn,fn) -> setFieldName (setTypeName $(return mdhE) tn) fn|]) (zipE tnEs (map (sToE' . nameBase) fnames))
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

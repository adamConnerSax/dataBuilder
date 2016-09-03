{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  DataBuilder.TH
-- Copyright   :  2016 Adam Conner-Sax
-- License     :  BSD3
--
-- Maintainer  :  adam_conner_sax@yahoo.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
module DataBuilder.TH
       (
         deriveBuilder
       , handleNothingL
       , handleJustL
       ) where

import qualified Data.Map                  as M
import           Language.Haskell.TH

import           Data.Char                 (toLower)
import           Data.Maybe                (fromJust)
import           DataBuilder.InternalTypes

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

getCons (DataD _ _ _ _ c _) = return c
getCons (NewtypeD _ _ _ _ c _) = return [c]
getCons (TySynD _ _ (ConT n)) = lookupType n
getCons (SigD _ (ConT n)) = lookupType n
getCons x = unsupported ("type in getCons " ++ show x)

typeVarName:: Type -> Maybe String
typeVarName (ConT n) = Just $ toLower <$> nameBase n
typeVarName (VarT n) = Just $ toLower <$> nameBase n
typeVarName (TupleT n) = Just $ "tuple" ++ show n
typeVarName ListT = Just $ "list"
typeVarName (AppT ListT t) = typeVarName t >>= \x->Just ("listOf" ++ x)
typeVarName (AppT t1 t2) = do
  ts1 <- typeVarName t1
  ts2 <- typeVarName t2
  return $ (toLower <$> ts1) ++ "_" ++ ts2
typeVarName _ = Nothing

typePretty :: Type -> Maybe String
typePretty (ConT n) = Just $ nameBase n
typePretty (VarT n) = Just $ nameBase n
typePretty (TupleT n) = Just $ show n ++ "-tuple"
typePretty ListT = Just $ "[]"
typePretty (AppT ListT t) = typePretty t >>= \x->Just ("[" ++ x ++ "]")
typePretty (AppT t1 t2) = do
  ts1 <- typePretty t1
  ts2 <- typePretty t2
  return $ ts1 ++ " " ++ ts2
typePretty _ = Nothing


typeNameE :: Type -> Q Exp
typeNameE x = maybe (unsupported ("type in typeName " ++ show x)) sToE' (typePretty x)

lookupType :: Name -> Q [Con]
lookupType n = do
  conInfo <- reify n
  case conInfo of
    TyConI dec -> getCons dec
    _          -> unsupported ("reify of " ++ show n ++ " returned other than TyConI.")

deriveBuilder::Name -> Name -> Q [Dec]
deriveBuilder builderName typeName = do
  [d|instance Builder $(conT builderName) $(conT typeName) where
       buildA mFN Nothing  = $(handleNothingL typeName) mFN
       buildA mFN (Just x) = $(handleJustL typeName) mFN x|]

handleNothingL::Name->Q Exp
handleNothingL n = do
  mfN <- newName "mf"
  let patsQ = [return $ VarP mfN]
      mfE = VarE mfN
  lamE patsQ (handleNothing n mfE)

handleNothing::Name->Exp->Q Exp
handleNothing n mfE = do
  blankBuildersMap <- buildAllBlankBuilders n mfE
  [e|internalSum $(listE . snd . unzip . M.toList $ blankBuildersMap)|]

handleJustL::Name->Q Exp
handleJustL n = do
  mfN <- newName "mf"
  aN <- newName "a"
  let patsQ = return <$> [VarP mfN, VarP aN]
      mfE = VarE mfN
      aE = VarE aN
  lamE patsQ (handleJust n mfE aE)

handleJust::Name->Exp->Exp->Q Exp
handleJust n mfE varAE = do
  cons <- lookupType n
  blankBuildersMap <- buildAllBlankBuilders n mfE
  let matchBuilder con = buildCaseMatch mfE con blankBuildersMap
  matches <- mapM matchBuilder cons
  return $ CaseE varAE matches

buildAllBlankBuilders::Name->Exp->Q (M.Map ConId ExpQ)
buildAllBlankBuilders n mfE = do
  cons <- lookupType n
  let first (x,_,_) = x
  cnames <- (fmap first) <$> (mapM conNameAndTypes cons)
  let bldrs = map (buildBlankBuilder mfE) cons
      cids = map nameBase cnames
  return $ M.fromList (zip cids bldrs)

sToE::String->Exp
sToE = LitE . StringL

sToE'::String->ExpQ
sToE' = litE . stringL

zipE::[ExpQ]->[ExpQ]->[ExpQ]
zipE e1s e2s = (tupE . (\(x,y)->[x,y])) <$> (zip e1s e2s)

builderPre::Exp->Con->Q (Name,[Type],ExpQ,ExpQ,[ExpQ])
builderPre mfE c = do
  (n,tl,mFNs) <- conNameAndTypes c
  let conMetaE = tupE [sToE' $ nameBase n,return mfE]
      conFE = [e|pure $(conE n)|]
      mfEs = case mFNs of
        Nothing -> take (length tl) $ repeat [e|Nothing|]
        Just fnames -> map (appE [e|Just|] . sToE' . nameBase) fnames
  return (n,tl,conMetaE,conFE,mfEs)

buildBlankBuilder::Exp->Con->Q Exp
buildBlankBuilder mfE c = do
  (n,tl,conMetaE,conFE,mfEs) <- builderPre mfE c
  let bldrs = map (appE [e|flip buildA Nothing|]) mfEs
      bldr = foldl (\e1 e2 -> [e|(<*>)|] `appE` e1 `appE` e2) conFE bldrs --This has to fold over Exps, otherwise bApply has multiple types during the fold
  [e|MDWrapped False $conMetaE $bldr|]


buildCaseMatch::Exp->Con->M.Map ConId ExpQ->Q Match
buildCaseMatch mfE c builderMap = do
  (n,tl,conMetaE,conFE,mfEs) <- builderPre mfE c
  ns <- mapM (\ty->newName $ fromJust (typeVarName ty)) tl
  let bldrs = map (appE [e|\(mf,v)-> buildA mf (Just v)|]) (zipE mfEs (varE <$> ns))
      bldr = foldl (\e1 e2 -> [e|(<*>)|] `appE` e1 `appE` e2) conFE bldrs --This has to fold over Exps, otherwise bApply has multiple types during the fold
      mdwE = [e|MDWrapped True $conMetaE $bldr|]
      newMap = M.insert (nameBase n) mdwE builderMap
      summedE = [e|internalSum $(listE . snd . unzip . M.toList $ newMap)|]
  match (conP n (varP <$> ns)) (normalB summedE) []

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module DataBuilderTH where

import Control.Monad
import Data.Functor.Identity
import Control.Applicative (liftA2)
import Language.Haskell.TH
import qualified Data.Map as M

import Data.Char (toLower)
import Control.Exception (throwIO,Exception)
import Data.Typeable (Typeable)
import Data.Data (toConstr,dataTypeName,dataTypeOf,Data)

data BuilderException = BuilderException String deriving (Show,Typeable)
instance Exception BuilderException

type TypeName = String
type FieldName = String
type ConName = String
data Metadata = Metadata { typeName::TypeName, conName::ConName, fieldName::Maybe FieldName} 

gatherMetaData::Data a=>a->Metadata
gatherMetaData a = let c = toConstr a in Metadata (dataTypeName $ dataTypeOf a) (show c) Nothing

type ConId = String
--data SumConstructor m f a = SumConstructor { dflt::Bool, builder::MFM m f a }


class Buildable f where
  -- inject and apply are exactly Applicative, inject=pure and apply=(<*>)
  bInject::Metadata-> a -> f a
  bApply::f (a->b) -> f a -> f b
  -- nothing and sum are *close* to MonadPlus, nothing = mzero and sum ~ foldl mplus nothing but we need to combine all at once
  bNothing::f a
  bSum::[f a]->f a
  bMeta::f a -> Metadata
  bIsNothing::f a -> Bool

{-- An example

data BuilderEx a = BuilderEx (Maybe MetaData) (IO (Maybe a)) 

sumBEs::[BuilderEx a]->BuilderEx a
sumBEs bes = case (length bes) of
  0 -> PutStrLn "No constructors in sumBEs" >> return Nothing
  1 -> head bes
  _ -> do
    let conName (BuilderEx (Metadata _ con _)) = con
        toName be = conName be ++ if not bIsnothing be then "*" else ""
        names = intercalate "," $ map toName bes
        prompt = "Type has multiple constructors. Please choose (" ++ names ++ "): "
    putStr prompt
    hFlush stdout
    cid <- getLine
    let mBe = find (\be->conName be == cid) bes
      case mBe of
        Nothing -> putStrLn (cid ++ " unrecognized constructor!") >> return Nothing
        Just be -> be

instance Buildable BuilderEx where
  bInject md x = BuilderEx md (return (Just x))
  bApply (BuilderEx mAB iomAB) (BuilderEx mA iomA) = Builder mAB iomB where
    iomB = do
      mf <- iomAB
      ma <- iomA
      return $ mf <*> ma
  bNothing = BuilderEx Nothing Nothing
  bSum = sumBEs
  bMeta (BuilderEx md _) = md
  bIsNothing (BuilderEx _ ma) = (ma == return Nothing) ??
   
-}

--String is field name, if present, in enclosing data type.
class Builder f a where
  buildM::Buildable f=>Maybe FieldName->Maybe a-> f a

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

lookupType :: Name -> Q [Con]
lookupType n = do
  conInfo <- reify n
  case conInfo of
    TyConI dec -> getCons dec
    _          -> unsupported ("reify of " ++ show n ++ " returned other than TyConI.")

deriveBuild::Name -> Q [Dec]
deriveBuild name = do
  [d|instance Buildable f=>Builder f $(conT name) where
       buildM mFn Nothing  = $(handleNothingL name) mFn
       buildM mFn (Just x) = $(handleJustL name) mFn x|]

handleNothingL::Name->Q Exp
handleNothingL n = do
  mFnN <- newName "mFn"
  let patsQ = [return $ VarP mFnN]
      mFnE = VarE mFnN
  lamE patsQ (handleNothing n mFnE)

handleNothing::Name->Exp->Q Exp
handleNothing n mFnE = do
  sumFE <- [e|\x -> bSum x|]
  blankBuildersMap <- buildAllBlankBuilders n mFnE
  let summedE = sumFE `AppE` (ListE . snd . unzip . M.toList $ blankBuildersMap)
  return $ summedE

handleJustL::Name->Q Exp
handleJustL n = do
  mFnN <- newName "mFn"
  aN <- newName "a"
  let patsQ = return <$> [VarP mFnN, VarP aN]
      mFnE = VarE mFnN        
      aE = VarE aN
  lamE patsQ (handleJust n mFnE aE) 

handleJust::Name->Exp->Exp->Q Exp
handleJust n mFnE varAE = do
  cons <- lookupType n
  blankBuildersMap <- buildAllBlankBuilders n mFnE
  let matchBuilder con = buildCaseMatch (nameBase n) mFnE con blankBuildersMap 
  matches <- mapM matchBuilder cons
  return $ CaseE varAE matches

buildAllBlankBuilders::Name->Exp->Q (M.Map ConId Exp) 
buildAllBlankBuilders n mFnE = do
  cons <- lookupType n
  let first (x,_,_) = x
  cnames <- (fmap first) <$> (mapM conNameAndTypes cons)
  bldrs <- mapM (buildBlankBuilder (nameBase n) mFnE) cons
  let cids = map nameBase cnames
  return $ M.fromList (zip cids bldrs)

sToE::String->Exp
sToE = LitE . StringL

buildCaseMatch::TypeName->Exp->Con->M.Map ConId Exp->Q Match
buildCaseMatch typeN mFnE c builderMap = do
  nothingE <- [e|Nothing|]
  justE <- [e|Just|] 
  bldME <- [e|buildM|]
  applyE  <- [e|bApply|]
  injectE <- [e|bInject|]
  sumFE <- [e|\x -> bSum x|]
  (n,tl,mFN) <- conNameAndTypes c
  ns <- mapM (\(ConT x)->newName $ toLower <$> (nameBase x)) tl
  metaCE <- [e|Metadata|]
  let metaE = metaCE `AppE` (sToE typeN) `AppE` (sToE (nameBase n)) `AppE` mFnE 
  let pats = VarP <$> ns
      vars = VarE <$> ns
      bldrs = case mFN of 
        Nothing -> map (\x->bldME `AppE` nothingE `AppE` (justE `AppE` x)) vars -- [Exp], of type MFM m f a
        Just fNames -> map (\(fn,vE)->bldME `AppE` (justE `AppE` (sToE $ nameBase fn)) `AppE` (justE `AppE` vE)) (zip fNames vars)
      conFE = injectE `AppE` metaE `AppE` (ConE n)
      bldr = foldl (\e1 e2 -> applyE `AppE` e1 `AppE` e2) conFE bldrs
      newMap = M.insert (nameBase n) bldr builderMap
      summedE = sumFE `AppE` (ListE . snd . unzip . M.toList $ newMap)
  return $ Match (ConP n pats) (NormalB summedE) []

buildBlankBuilder::TypeName->Exp->Con->Q Exp
buildBlankBuilder typeN mFnE c = do
  nothingE <- [e|Nothing|]
  justE <- [e|Just|]
  bldME <- [e|buildM|]
  applyE  <- [e|bApply|]
  injectE <- [e|bInject|]
  (n,tl,mFNs) <- conNameAndTypes c
  let bldrs = case mFNs of
        Nothing -> replicate (length tl) (bldME `AppE` nothingE `AppE` nothingE) -- [Exp], of type MFM m f a
        Just fNames -> map (\n->bldME `AppE` (justE `AppE` (sToE $ nameBase n)) `AppE` nothingE) fNames
  metaCE <- [e|Metadata|]
  let metaE = metaCE `AppE` (sToE typeN) `AppE` (sToE $ nameBase n) `AppE` mFnE 
  let conFE = injectE `AppE` metaE `AppE` (ConE n)
      foldBldrs = foldl (\e1 e2 -> applyE `AppE` e1 `AppE` e2) conFE bldrs 
  return $ foldBldrs



{-
type MFM m f a = m (f (Maybe a))

class ApplicativeLike f where
  alInject::a->f a
  alApply::f (x->y)->f x->f y

instance Applicative f=>ApplicativeLike f where
  alInject = pure
  alApply = (<*>)

class FM m f where
  injectMfM::a -> MFM m f a
  applyMfM::MFM m f (x->y)->MFM m f x->MFM m f y

instance (Monad m, ApplicativeLike f)=>FM m f where
  injectMfM x = return $ (alInject (Just x))
  applyMfM mfmFxy mfmX = do
    fmFXY <- mfmFxy
    fmX <- mfmX
    return $ alApply (alApply (alInject (<*>)) fmFXY) fmX

type ConId = String
data SumConstructor m f a = SumConstructor { dflt::Bool, builder::MFM m f a }

class ConSummable m f where
  sumF::[(ConId,SumConstructor m f a)] -> MFM m f a 

class Builder m f a where
  buildM::(Monad m,ApplicativeLike f,ConSummable m f)=>Maybe a->MFM m f a


deriveBuild::Name -> Q [Dec]
deriveBuild name = do
  [d|instance Builder IO Identity $(conT name) where
       buildM Nothing  = $(handleNothingL name)
       buildM (Just x) = $(handleJustL name) x|]

handleNothingL::Name->Q Exp
handleNothingL n = do
  sumFE <- [e|\x -> sumF x|]
  blankBuildersMap <- buildAllBlankBuilders n
  let summedE = sumFE `AppE` (ListE $ map (\(cid,bldrE) -> TupE [LitE $ StringL cid,bldrE]) (M.toList blankBuildersMap))
  return $ summedE

handleJustL::Name->Q Exp
handleJustL n = do
  aN <- newName "a"
  let patQ = return $ VarP aN
      aE = VarE aN
  lamE [patQ] (handleJust aE n) 

handleJust::Exp->Name->Q Exp
handleJust varAE n = do
  cons <- lookupType n
  blankBuildersMap <- buildAllBlankBuilders n
  matches <- mapM (flip buildCaseMatch blankBuildersMap) cons
  return $ CaseE varAE matches

buildAllBlankBuilders::Name->Q (M.Map ConId Exp) -- that Exp is SumConstructor False 
buildAllBlankBuilders n = do
  cons <- lookupType n
  cnames <- (fmap fst) <$> (mapM conNameAndTypes cons)
  sumCE <- [e|\bldr->SumConstructor False bldr|] 
  bldrs <- mapM buildBlankBuilder cons
  let scs = map (\e->sumCE `AppE` e) bldrs
  let cids = map nameBase cnames
  return $ M.fromList (zip cids scs)


buildCaseMatch::Con->M.Map ConId Exp->Q Match
buildCaseMatch c builderMap = do
  just <- [e|Just|] 
  bldME <- [e|buildM|]
  applyE  <- [e|applyMfM|]
  injectE <- [e|injectMfM|]
  sumCE <- [e|\bldr->SumConstructor True bldr|]
  sumFE <- [e|\x -> sumF x|]
  (n,tl) <- conNameAndTypes c
  ns <- mapM (\(ConT x)->newName $ toLower <$> (nameBase x)) tl
  let pats = VarP <$> ns
      vars = VarE <$> ns
      bldrs = map (\x->bldME `AppE` (just `AppE` x)) vars -- [Exp], of type MFM m f a
      conFE = injectE `AppE` (ConE n)
      bldr = foldl (\e1 e2 -> applyE `AppE` e1 `AppE` e2) conFE bldrs
      sc = sumCE `AppE` bldr
      newMap = M.insert (nameBase n) sc builderMap
      summedE = sumFE `AppE` (ListE $ map (\(cid,bldrE) -> TupE [LitE $ StringL cid,bldrE]) (M.toList newMap))
  return $ Match (ConP n pats) (NormalB summedE) []

buildBlankBuilder::Con->Q Exp
buildBlankBuilder c = do
  nothingE <- [e|Nothing|]
  bldME <- [e|buildM|]
  applyE  <- [e|applyMfM|]
  injectE <- [e|injectMfM|]
  (n,tl) <- conNameAndTypes c
  let bldrs = replicate (length tl) (bldME `AppE` nothingE) -- [Exp], of type MFM m f a
      conFE = injectE `AppE` (ConE n)
      foldBldrs = foldl (\e1 e2 -> applyE `AppE` e1 `AppE` e2) conFE bldrs 
  return $ foldBldrs

-}

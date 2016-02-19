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

data BuilderException = BuilderException String deriving (Show,Typeable)
instance Exception BuilderException


unsupported :: String -> Q a
unsupported msg = fail ("Unsupported type: " ++ show msg)

conNameAndTypes::Con->Q (Name,[Type])
conNameAndTypes (NormalC n ts) = return (n, snd <$> ts)
conNameAndTypes (RecC n vts) = let thrd (_,_,x) = x  in return (n,thrd <$> vts)
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


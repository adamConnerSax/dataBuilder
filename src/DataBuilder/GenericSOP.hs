{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
-----------------------------------------------------------------------------
--
-- Module      :  DataBuilder.GenericSOP
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

module DataBuilder.GenericSOP
       (
         module GSOP --re-export the Generics.SOP classes for deriving
       , module Generics.SOP.TH
       ) where

import qualified Data.Map                  as M
import           Data.Maybe                (fromJust)
import           Generics.SOP              hiding (FieldName)
import           Generics.SOP              as GSOP (Generic, HasDatatypeInfo)
import           Generics.SOP.TH           (deriveGeneric)
import qualified GHC.Generics              as GHC
--
import           DataBuilder.InternalTypes

ci2name::ConstructorInfo xs-> ConName
ci2name (Constructor cn) = cn
ci2name (Infix cn _ _) = cn
ci2name (Record cn _) = cn

ci2RecordNames::ConstructorInfo xs -> Maybe (NP FieldInfo xs)
ci2RecordNames (Record _ fi) = Just fi
ci2RecordNames _ = Nothing


ci2md::Maybe FieldName->DatatypeName->ConstructorInfo xs->K (ConName,Maybe FieldName) xs
ci2md mf tn ci = K $ (ci2name ci,mf)


gMakeMDs::SListI2 xss=>Maybe FieldName->DatatypeName->NP ConstructorInfo xss ->[(ConName,Maybe FieldName)]
gMakeMDs mf tn cs = hcollapse $ hliftA (ci2md mf tn) cs

type GenericSOPC a = (Generic a, HasDatatypeInfo a)

fi2mf::FieldInfo a->Maybe FieldName
fi2mf (FieldInfo x) = Just x

type MdwMap f err a = M.Map ConName (MDWrapped f err a)

mdwMapToList::MdwMap f err a->[MDWrapped f err a]
mdwMapToList = snd . unzip . M.toList

mdwMapFromList::[MDWrapped f err a]->MdwMap f err a
mdwMapFromList mdws = M.fromList $ zip ((fst . metadata) <$> mdws) mdws

type GBuilderTopC f err a = (Buildable f err, GenericSOPC a, All2 (Builder f err) (Code a))

instance GBuilderTopC f err a=>GBuilder f err a where
  gBuildA mf ma = case ma of
    Nothing -> internalSum $ buildBlanks mf
    Just x  -> let cn = (constructorName x) in internalSum . snd . unzip . M.toList $ M.insert cn (buildDefaulted mf x) (buildBlankMap mf)

buildBlankMap::forall f err a.GBuilderTopC f err a => Maybe FieldName->MdwMap f err a
buildBlankMap = mdwMapFromList . buildBlanks


buildBlanks::forall f err a.GBuilderTopC f err a => Maybe FieldName->[MDWrapped f err a]
buildBlanks mf =
    let (tn,cs) = case datatypeInfo (Proxy :: Proxy a) of
          ADT _ tn cs -> (tn,cs)
          Newtype _ tn c -> (tn,(c :* Nil))
        mds = gMakeMDs mf tn cs
        builders = fmap to <$> buildBlanks' mf tn cs
        mbs = zip mds builders
        makeMDW (md',bldr) = MDWrapped False md' bldr
    in makeMDW <$> mbs

--type All2C f xss = (All2 (Builder f) xss)
type GBuilderC2 f err xss = (Buildable f err, All2 (Builder f err) xss, SListI2 xss)

buildBlanks'::forall f err xss.GBuilderC2 f err xss => Maybe FieldName->DatatypeName->NP ConstructorInfo xss->[(FValidation f err) (SOP I xss)]
buildBlanks' mf tn cs =
  let allBuilder = Proxy :: Proxy (All (Builder f err))
      pop = POP $ hcliftA allBuilder (buildBlank mf tn) cs
--      wrapped = hliftA wrapBuildable pop -- POP (FABuildable f) xss
      sop = apInjs_POP pop -- [SOP f xss]
  in hsequence <$> sop -- [f (SOP I xss)]


type GBuilderC1 f err xs  = (Buildable f err, All (Builder f err) xs, SListI xs)

buildBlank::forall f err xs.GBuilderC1 f err xs => Maybe FieldName->DatatypeName->ConstructorInfo xs->NP (FValidation f err) xs
buildBlank mf tn ci =
    let fieldNames = ci2RecordNames ci
        builderC = Proxy :: Proxy (Builder f err)
    in case fieldNames of
           Nothing -> hcpure builderC (buildA Nothing Nothing)
           Just fns ->
             let builder::Builder f err a=>FieldInfo a -> FValidation f err a
                 builder fi = buildA (fi2mf fi) Nothing
             in hcliftA builderC builder fns

buildDefaulted::forall f err a.GBuilderTopC f err a => Maybe FieldName->a->MDWrapped f err a
buildDefaulted mf a =
  let cn = constructorName a
--      mdBase = setmConName mcn md
      allBuilder = Proxy :: Proxy (All (Builder f err))
--      dHNP = unAll_NP $ unAll2 Dict :: NP (Dict (All HasDatatypeInfo)) (Code a)
      (tn,cs) = case datatypeInfo (Proxy :: Proxy a) of
        ADT _ tn cs -> (tn,cs)
        Newtype _ tn c -> (tn,(c :* Nil))
      sopf   = SOP $ hcliftA2 allBuilder (buildDefFromConInfo mf tn) cs (unSOP $ from a) -- SOP f xss
--      sopFAf = hliftA wrapBuildable sopf                                                   -- SOP (FABuilder f) xss
      fa = fmap to . hsequence $ sopf                                           -- f a
  in MDWrapped True (cn,mf) fa


buildDefFromConInfo::forall f err xs.GBuilderC1 f err xs=>Maybe FieldName->DatatypeName->ConstructorInfo xs->NP I xs->NP (FValidation f err) xs
buildDefFromConInfo md tn ci args =
  let fieldNames = ci2RecordNames ci
      builderC = Proxy :: Proxy (Builder f err)
  in case fieldNames of
      Nothing ->
        let builder::Builder f err a=>I a -> FValidation f err a
            builder ia = buildA Nothing (Just $ unI ia)
        in hcliftA builderC builder args
      Just fns ->
        let builder::Builder f err a=>FieldInfo a->I a->FValidation f err a
            builder fi ia = buildA (fi2mf fi) (Just (unI ia))
        in hcliftA2 builderC builder fns args

constructorName::forall a.GenericSOPC a=>a->ConName
constructorName a =
  let cs = case datatypeInfo (Proxy :: Proxy a) of
        ADT _ tn cs -> cs
        Newtype _ tn c -> c :* Nil
      getConName::ConstructorInfo xs->NP I xs->K ConName xs
      getConName ci args = K $ ci2name ci
  in hcollapse $ hliftA2 getConName cs (unSOP $ from a)


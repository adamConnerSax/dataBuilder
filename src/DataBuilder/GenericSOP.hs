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
       )where


import qualified Data.Map                  as M
import           Data.Maybe                (fromJust)
import           Data.Semigroup            (Semigroup)
import           Generics.SOP              hiding (FieldName, constructorName)
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

type MdwMap f e a = M.Map ConName (MDWrapped f e a)

mdwMapToList::MdwMap f e a->[MDWrapped f e a]
mdwMapToList = snd . unzip . M.toList

mdwMapFromList::[MDWrapped f e a]->MdwMap f e a
mdwMapFromList mdws = M.fromList $ zip ((fst . metadata) <$> mdws) mdws

type GBuilderTopC f e a = (Buildable f e, GenericSOPC a, All2 (Builder f e) (Code a))

instance (Semigroup e, GBuilderTopC f e a)=>GBuilder f e a where
  gBuildA va mf ma = case ma of
    Nothing -> internalSum $ buildBlanks va mf
    Just x  -> let cn = (constructorName x) in internalSum . snd . unzip . M.toList $ M.insert cn (buildDefaulted va mf x) (buildBlankMap va mf)

buildBlankMap::forall f e a.(Semigroup e, GBuilderTopC f e a) => Validator e a->Maybe FieldName->MdwMap f e a
buildBlankMap va = mdwMapFromList . buildBlanks va

validateVF::Functor f=>Validator e a -> VF f e a -> VF f e a
validateVF va = VF . (fmap mergeAV) . unVF . fmap va

buildBlanks::forall f e a.(Semigroup e, GBuilderTopC f e a) =>Validator e a->Maybe FieldName->[MDWrapped f e a]
buildBlanks va mf =
    let (tn,cs) = case datatypeInfo (Proxy :: Proxy a) of
          ADT _ tn cs -> (tn,cs)
          Newtype _ tn c -> (tn,(c :* Nil))
        mds = gMakeMDs mf tn cs
        builders = (validateVF va . fmap to) <$> buildBlanks' mf tn cs -- here's where we validate the constructed type (1)
        mbs = zip mds builders
        makeMDW (md',bldr) = MDWrapped False md' bldr
    in makeMDW <$> mbs

--type All2C f xss = (All2 (Builder f) xss)
type GBuilderC2 f e xss = (Buildable f e, All2 (Builder f e) xss, SListI2 xss)
buildBlanks'::forall f e xss.(Semigroup e, GBuilderC2 f e xss) => Maybe FieldName->DatatypeName->NP ConstructorInfo xss->[VF f e (SOP I xss)]
buildBlanks' mf tn cs =
  let allBuilder = Proxy :: Proxy (All (Builder f e))
      pop = POP $ hcliftA allBuilder (buildBlank mf tn) cs
--      wrapped = hliftA wrapBuildable pop -- POP (FABuildable f) xss
      sop = apInjs_POP pop -- [SOP f xss]
  in hsequence <$> sop -- [f (SOP I xss)]


type GBuilderC1 f e xs  = (Buildable f e, All (Builder f e) xs, SListI xs)
buildBlank::forall f e xs.GBuilderC1 f e xs => Maybe FieldName->DatatypeName->ConstructorInfo xs->NP (VF f e) xs
buildBlank mf tn ci =
    let fieldNames = ci2RecordNames ci
        builderC = Proxy :: Proxy (Builder f e)
    in case fieldNames of
           Nothing -> hcpure builderC (buildA Nothing Nothing)
           Just fns ->
             let builder::Builder f e a=>FieldInfo a -> VF f e a
                 builder fi = buildA (fi2mf fi) Nothing
             in hcliftA builderC builder fns

buildDefaulted::forall f e a.(Semigroup e, GBuilderTopC f e a) => Validator e a->Maybe FieldName->a->MDWrapped f e a
buildDefaulted va mf a =
  let cn = constructorName a
--      mdBase = setmConName mcn md
      allBuilder = Proxy :: Proxy (All (Builder f e))
--      dHNP = unAll_NP $ unAll2 Dict :: NP (Dict (All HasDatatypeInfo)) (Code a)
      (tn,cs) = case datatypeInfo (Proxy :: Proxy a) of
        ADT _ tn cs -> (tn,cs)
        Newtype _ tn c -> (tn,(c :* Nil))
      sopf   = SOP $ hcliftA2 allBuilder (buildDefFromConInfo mf tn) cs (unSOP $ from a) -- SOP (VF f e) xss
--      sopFAf = hliftA wrapBuildable sopf                                                   -- SOP (FABuilder f) xss
      vfa = validateVF va . fmap to . hsequence $ sopf   -- here's where we validate the constructed type (2) 
  in MDWrapped True (cn,mf) vfa


buildDefFromConInfo::forall f e xs.GBuilderC1 f e xs=>Maybe FieldName->DatatypeName->ConstructorInfo xs->NP I xs->NP (VF f e) xs
buildDefFromConInfo md tn ci args =
  let fieldNames = ci2RecordNames ci
      builderC = Proxy :: Proxy (Builder f)
  in case fieldNames of
      Nothing ->
        let builder::Builder f e a=>I a -> VF f e a
            builder ia = buildA Nothing (Just $ unI ia)
        in hcliftA builderC builder args
      Just fns ->
        let builder::Builder f e a=>FieldInfo a->I a->VF f e a
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

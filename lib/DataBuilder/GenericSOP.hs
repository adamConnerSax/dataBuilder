{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import qualified GHC.Generics as GHC
import Generics.SOP 
import Generics.SOP as GSOP (Generic,HasDatatypeInfo)
import Generics.SOP.Dict
import Generics.SOP.TH (deriveGeneric)
import qualified Data.Map as M
import Data.Maybe (fromJust)
--
import DataBuilder.InternalTypes

ci2name::ConstructorInfo xs-> ConName
ci2name (Constructor cn) = cn
ci2name (Infix cn _ _) = cn
ci2name (Record cn _) = cn

ci2RecordNames::ConstructorInfo xs -> Maybe (NP FieldInfo xs)
ci2RecordNames (Record _ fi) = Just fi
ci2RecordNames _ = Nothing

ci2md::Metadata->DatatypeName->ConstructorInfo xs->K Metadata xs
ci2md md tn ci = K $ setmConName (Just $ ci2name ci) . setTypeName tn $ md

gAddMD::SListI2 xss=>Metadata->DatatypeName->NP ConstructorInfo xss ->[Metadata]
gAddMD md tn cs = hcollapse $ hliftA (ci2md md tn) cs

type GenericSOPC a = (Generic a, HasDatatypeInfo a)
--type BuildableC f = Buildable f 

setFromFieldInfo::forall a.FieldInfo a->Metadata->Metadata
setFromFieldInfo fi md = let (FieldInfo fName) = fi in setmFieldName (Just fName) md

setFromTypeName::forall a.K DatatypeName a->Metadata->Metadata
setFromTypeName tn md = setTypeName (unK tn) md

setTypeAndFieldNames::forall a.K DatatypeName a->FieldInfo a->Metadata->Metadata
setTypeAndFieldNames tn fi md = setFromFieldInfo fi . setFromTypeName tn $ md

--setmConName::forall a g.HasMetadata g=>Maybe ConName->g->g
--setmConName mcn mdh = setMetadata (Metadata (getTypeName $ getMetadata mdh) mcn (getmFieldName $ getMetadata mdh)) mdh

type MdwMap f a = M.Map ConName (MDWrapped f a)

mdwMapToList::MdwMap f a->[MDWrapped f a]
mdwMapToList = snd . unzip . M.toList

mdwMapFromList::[MDWrapped f a]->MdwMap f a
mdwMapFromList mdws = M.fromList $ zip ((fromJust . conName . metadata) <$> mdws) mdws

type GBuilderTopC f a = (Buildable f, GenericSOPC a, All2 (Builder f) (Code a), All2 HasDatatypeInfo (Code a))

instance GBuilderTopC f a=>GBuilder f a where
  gBuildA md ma = case ma of
    Nothing -> internalSum $ buildBlanks md
    Just x  -> let cn = fromJust (constructorName x) in internalSum . snd . unzip . M.toList $ M.insert cn (buildDefaulted md x) (buildBlankMap md)

buildBlankMap::forall f a.GBuilderTopC f a => Metadata->MdwMap f a
buildBlankMap = mdwMapFromList . buildBlanks


buildBlanks::forall f a.GBuilderTopC f a => Metadata->[MDWrapped f a]
buildBlanks md = 
    let (tn,cs) = case datatypeInfo (Proxy :: Proxy a) of
          ADT _ tn cs -> (tn,cs)
          Newtype _ tn c -> (tn,(c :* Nil))
        mds = gAddMD md tn cs
        builders = (unFA . fmap to) <$> buildBlanks' md tn cs
        mbs = zip mds builders
        makeMDW (md',bldr) = MDWrapped False md' bldr
    in makeMDW <$> mbs

type All2C f xss = (All2 (Builder f) xss, All2 HasDatatypeInfo xss)
type GBuilderC2 f xss = (Buildable f, All2C f xss, SListI2 xss)
buildBlanks'::forall f xss.GBuilderC2 f xss => Metadata->DatatypeName->NP ConstructorInfo xss->[(FABuildable f) (SOP I xss)]
buildBlanks' md tn cs =
  let dHNP = unAll_NP $ unAll2 Dict :: NP (Dict (All HasDatatypeInfo)) xss
      allBuilder = Proxy :: Proxy (All (Builder f))
      pop = POP $ hcliftA2 allBuilder (\d->withDict d (buildBlank md tn)) dHNP cs
      wrapped = hliftA wrapBuildable pop -- POP (FABuildable f) xss
      sop = apInjs_POP wrapped -- [SOP (FABuildable f) xss]
  in hsequence <$> sop -- [(FABuildable f) (SOP I xss)]


type GBuilderC1 f xs  = (Buildable f, All (Builder f) xs, All HasDatatypeInfo xs, SListI xs)
buildBlank::forall f xs.GBuilderC1 f xs => Metadata->DatatypeName->ConstructorInfo xs->NP f xs
buildBlank mdh tn ci = 
    let mdBase = Metadata tn (Just $ ci2name ci) Nothing
        fieldNames = ci2RecordNames ci
        typeNames = gTypeNames :: NP (K DatatypeName) xs
        builderC = Proxy :: Proxy (Builder f)
    in case fieldNames of 
           Nothing ->
             let builder::Builder f a=>K DatatypeName a -> f a
                 builder tn = buildA (setFromTypeName tn mdBase) Nothing
             in hcliftA builderC builder typeNames
           Just fns -> 
             let builder::Builder f a=>FieldInfo a -> K DatatypeName a-> f a
                 builder fi ktn = buildA (setTypeAndFieldNames ktn fi mdBase) Nothing
             in hcliftA2 builderC builder fns typeNames

buildDefaulted::forall f a.GBuilderTopC f a => Metadata->a->MDWrapped f a
buildDefaulted md a =
  let mcn = constructorName a
      mdBase = setmConName mcn md
      allBuilder = Proxy :: Proxy (All (Builder f))
      dHNP = unAll_NP $ unAll2 Dict :: NP (Dict (All HasDatatypeInfo)) (Code a)
      (tn,cs) = case datatypeInfo (Proxy :: Proxy a) of
        ADT _ tn cs -> (tn,cs)
        Newtype _ tn c -> (tn,(c :* Nil))
      sopf   = SOP $ hcliftA3 allBuilder (\d -> withDict d (buildDefFromConInfo mdBase tn)) dHNP cs (unSOP $ from a) -- SOP f xss
      sopFAf = hliftA wrapBuildable sopf                                                   -- SOP (FABuilder f) xss
      fa = unFA $ (fmap to) . hsequence $ sopFAf                                           -- f a
  in MDWrapped True mdBase fa


buildDefFromConInfo::forall f xs.GBuilderC1 f xs=>Metadata->DatatypeName->ConstructorInfo xs->NP I xs->NP f xs
buildDefFromConInfo md tn ci args =
  let mdBase = Metadata tn (Just $ ci2name ci) Nothing 
      fieldNames = ci2RecordNames ci
      typeNames = gTypeNames :: NP (K DatatypeName) xs
      builderC = Proxy :: Proxy (Builder f)
  in case fieldNames of
      Nothing ->
        let builder::Builder f a=>K DatatypeName a -> I a -> f a
            builder ktn ia = buildA (setFromTypeName ktn mdBase) (Just $ unI ia)
        in hcliftA2 builderC builder typeNames args
      Just fns ->
        let builder::Builder f a=>FieldInfo a->K DatatypeName a->I a->f a
            builder fi ktn ia = buildA (setTypeAndFieldNames ktn fi mdBase) (Just (unI ia))
        in hcliftA3 builderC builder fns typeNames args

constructorName::forall a.GenericSOPC a=>a->Maybe ConName
constructorName a =
  let cs = case datatypeInfo (Proxy :: Proxy a) of
        ADT _ tn cs -> cs
        Newtype _ tn c -> c :* Nil
      getConName::ConstructorInfo xs->NP I xs->K (Maybe ConName) xs
      getConName ci args = K $ Just (ci2name ci)
  in hcollapse $ hliftA2 getConName cs (unSOP $ from a)

gTypeName::forall a.HasDatatypeInfo a=>K DatatypeName a
gTypeName = case datatypeInfo (Proxy :: Proxy a) of
  ADT _ tn _ -> K tn
  Newtype _ tn _ -> K tn

gTypeNames::forall xs.(All HasDatatypeInfo xs, SListI xs)=>NP (K DatatypeName) xs
gTypeNames = hcpure (Proxy :: Proxy HasDatatypeInfo) gTypeName 


{- Generic and HasDatatypeInfo instances for a bunch of basic types -}
instance Generic Int
instance HasDatatypeInfo Int

instance Generic Float
instance HasDatatypeInfo Float

instance Generic Double
instance HasDatatypeInfo Double

--instance Generic String
--instance HasDatatypeInfo String

instance Generic Char
instance HasDatatypeInfo Char


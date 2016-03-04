{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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
       , gTypeName
       )where

import qualified GHC.Generics as GHC
import Generics.SOP 
import Generics.SOP as GSOP (Generic,HasDatatypeInfo)
import Generics.SOP.Constraint (SListIN(..),AllF)
import Generics.SOP.Dict
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
--
import DataBuilder.InternalTypes

ci2name::ConstructorInfo xs-> ConName
ci2name (Constructor cn) = cn
ci2name (Infix cn _ _) = cn
ci2name (Record cn _) = cn

ci2RecordNames::ConstructorInfo xs -> Maybe (NP FieldInfo xs)
ci2RecordNames (Record _ fi) = Just fi
ci2RecordNames _ = Nothing

ci2md::HasMetadata g=>g->DatatypeName->ConstructorInfo xs->K g xs
ci2md mdh tn ci = K $ setMetadata (Metadata tn (Just $ ci2name ci) (fieldName . getMetadata $ mdh)) mdh

gAddMD::(HasMetadata g,SListI2 xss)=>g->DatatypeName->NP ConstructorInfo xss ->[g]
gAddMD mdh tn cs = hcollapse $ hliftA (ci2md mdh tn) cs

type GenericSOPC a = (Generic a, HasDatatypeInfo a)
type BuildableC f g = (Buildable f g, HasMetadata g)

setFieldName::forall a g.HasMetadata g=>FieldInfo a->g->g
setFieldName fi mdh = let (FieldInfo fName) = fi in setMetadata ((getMetadata mdh)  { fieldName = Just fName }) mdh

setTypeName::forall g a.HasMetadata g=>K DatatypeName a->g->g
setTypeName tn mdh = setMetadata ((getMetadata mdh) { typeName = unK tn }) mdh

setTypeAndFieldNames::forall g a.HasMetadata g=>K DatatypeName a->FieldInfo a->g->g
setTypeAndFieldNames tn fi mdh = setFieldName fi (setTypeName tn mdh)

type MdwMap f g a = M.Map ConName (MDWrapped f g a)

mdwMapToList::MdwMap f g a->[MDWrapped f g a]
mdwMapToList = snd . unzip . M.toList

mdwMapFromList::HasMetadata g=>[MDWrapped f g a]->MdwMap f g a
mdwMapFromList mdws = M.fromList $ zip ((fromJust . conName . getMetadata) <$> mdws) mdws

--class (Builder f g a,HasDatatypeInfo a)=>GBuilderC f g a


type GBuilderTopC f g a = (BuildableC f g, GenericSOPC a, All2 (Builder f g) (Code a), All2 HasDatatypeInfo (Code a))

instance GBuilderTopC f g a=>GBuilder f g a where
  gBuildM mdh ma = case ma of
    Nothing -> internalSum $ buildBlanks mdh
    Just x  -> let cn = fromJust (constructorName x) in internalSum . snd . unzip . M.toList $ M.insert cn (buildDefaulted mdh x) (buildBlankMap mdh)

buildBlankMap::forall f g a.GBuilderTopC f g a => g->MdwMap f g a
buildBlankMap = mdwMapFromList . buildBlanks


buildBlanks::forall f g a.GBuilderTopC f g a => g->[MDWrapped f g a]
buildBlanks mdh = 
    let (tn,cs) = case datatypeInfo (Proxy :: Proxy a) of
          ADT _ tn cs -> (tn,cs)
          Newtype _ tn c -> (tn,(c :* Nil))
        mdhs = gAddMD mdh tn cs
        builders = (unFA . fmap to) <$> buildBlanks' mdh tn cs
        mbs = zip mdhs builders
        makeMDW (mdh',bldr) = MDWrapped False mdh' bldr
    in makeMDW <$> mbs

dictWith = flip withDict

type All2C f g xss = (All2 (Builder f g) xss, All2 HasDatatypeInfo xss)
type GBuilderC2 f g xss = (BuildableC f g, All2C f g xss, SListI2 xss)
buildBlanks'::forall f g xss.GBuilderC2 f g xss => g->DatatypeName->NP ConstructorInfo xss->[(FABuildable f) (SOP I xss)]
buildBlanks' mdh tn cs =
  let dH  = Dict :: Dict (All2 HasDatatypeInfo) xss
      dHNP = unAll_NP $ unAll2 dH -- NP (Dict (All HasDataTypeInfo)) xss
      allBuilder = Proxy :: Proxy (All (Builder f g))
      pop = POP $ hcliftA2 allBuilder (\d->withDict d (buildBlank mdh tn)) dHNP cs
      wrapped = hliftA wrapBuildable pop -- POP (FABuildable f) xss
      sop = apInjs_POP wrapped -- [SOP (FABuildable f) xss]
  in hsequence <$> sop -- [(FABuildable f) (SOP I xss)]


type GBuilderC1 f g xs  = (BuildableC f g, All (Builder f g) xs, All HasDatatypeInfo xs, SListI xs)
buildBlank::forall f g xs.GBuilderC1 f g xs => g->DatatypeName->ConstructorInfo xs->NP f xs
buildBlank mdh tn ci = 
    let mdhBase = setMetadata (Metadata tn (Just $ ci2name ci) Nothing) mdh
        fieldNames = ci2RecordNames ci
        typeNames = gTypeNames :: NP (K DatatypeName) xs
        allBuilder = Proxy :: Proxy (Builder f g)
    in case fieldNames of 
           Nothing -> let builder::Builder f g a=>K DatatypeName a -> f a
                          builder tn = buildM (setTypeName tn mdhBase) Nothing
                      in hcliftA allBuilder builder typeNames
           Just fns -> 
             let builder::Builder f g a=>FieldInfo a -> K DatatypeName a-> f a
                 builder fi ktn = buildM (setTypeAndFieldNames ktn fi mdhBase) Nothing
             in hcliftA2 allBuilder builder fns typeNames


buildDefaulted::forall f g a.GBuilderTopC f g a => g->a->MDWrapped f g a
buildDefaulted mdh a =
  let allBuilder = Proxy :: Proxy (All (Builder f g))
      dH  = Dict :: Dict (All2 HasDatatypeInfo) (Code a)
      dHNP = unAll_NP $ unAll2 dH -- NP (Dict (All HasDataTypeInfo)) xss
      (tn,cs) = case datatypeInfo (Proxy :: Proxy a) of
        ADT _ tn cs -> (tn,cs)
        Newtype _ tn c -> (tn,(c :* Nil))
      sopf   = SOP $ hcliftA3 allBuilder (\d -> withDict d (buildDefFromConInfo mdh tn)) dHNP cs (unSOP $ from a) -- SOP f xss
      sopFAf = hliftA wrapBuildable sopf                                                   -- SOP (FABuilder f) xss
      fa = unFA $ (fmap to) . hsequence $ sopFAf                                           -- f a
  in MDWrapped True mdh fa


buildDefFromConInfo::forall f g xs.GBuilderC1 f g xs=>g->DatatypeName->ConstructorInfo xs->NP I xs->NP f xs
buildDefFromConInfo mdh tn ci args =
  let mdhBase = setMetadata (Metadata tn (Just $ ci2name ci) Nothing) mdh
      fieldNames = ci2RecordNames ci
      typeNames = gTypeNames :: NP (K DatatypeName) xs
      builderC = Proxy :: Proxy (Builder f g)
  in case fieldNames of
      Nothing ->
        let builder::Builder f g a=>K DatatypeName a -> I a -> f a
            builder ktn ia = buildM (setTypeName ktn mdhBase) (Just $ unI ia)
        in hcliftA2 builderC builder typeNames args
      Just fns ->
        let builder::Builder f g a=>FieldInfo a->K DatatypeName a->I a->f a
            builder fi ktn ia = buildM (setTypeAndFieldNames ktn fi mdhBase) (Just (unI ia))
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


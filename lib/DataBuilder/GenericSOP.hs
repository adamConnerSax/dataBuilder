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
         module GSOP
       , getConstructorName
       )where

import qualified GHC.Generics as GHC
import Generics.SOP 
import Generics.SOP as GSOP (Generic,HasDatatypeInfo)
import Generics.SOP.Constraint (SListIN(..),AllF)
import qualified Data.Map as M
import Data.Maybe (fromJust)
--
import DataBuilder.InternalTypes


-- some types for testing
data TestNull = Null deriving (Show,GHC.Generic)
instance Generic TestNull
instance HasDatatypeInfo TestNull

data TestOne = One Int deriving (Show,GHC.Generic)
instance Generic TestOne
instance HasDatatypeInfo TestOne

data TestTwo = Two Int String deriving (Show,GHC.Generic)
instance Generic TestTwo
instance HasDatatypeInfo TestTwo

data TestRecord = TestR { intF::Int, stringF::String } deriving (Show,GHC.Generic)
instance Generic TestRecord
instance HasDatatypeInfo TestRecord

data TestSum = A | B Int | C Char Int | D Char Int TestRecord deriving (Show,GHC.Generic)
instance Generic TestSum
instance HasDatatypeInfo TestSum

data TestNested = Nested Int String TestSum deriving (Show,GHC.Generic)
instance Generic TestNested
instance HasDatatypeInfo TestNested

newtype TestNewT = TestNewT { unTest::TestSum } deriving (Show,GHC.Generic)
instance Generic TestNewT
instance HasDatatypeInfo TestNewT


{-
gBuildMD::forall g a.(HasMetadata g,Generic a, HasDatatypeInfo a)=>g->a->[g]
gBuildMD mdh _ = case datatypeInfo (Proxy :: Proxy a) of
  ADT _ typeName cs    -> gAddMD mdh typeName cs
  Newtype _ typeName c -> gAddMD mdh typeName (c :* Nil)
-}

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



--newMetadata::HasMetadata g=>g->DatatypeName->ConstructorInfo xs->g->g
--newMetadata tn ci mdh = setMetadata (Metadata tn (Just $ ci2name ci) (fieldName . getMetadata mdh))

addFieldName::forall a g.HasMetadata g=>FieldInfo a->g->g
addFieldName fi mdh = let (FieldInfo fName) = fi in setMetadata ((getMetadata mdh)  { fieldName = Just fName }) mdh


type MdwMap f g a = M.Map ConName (MDWrapped f g a)

mdwMapToList::MdwMap f g a->[MDWrapped f g a]
mdwMapToList = snd . unzip . M.toList

mdwMapFromList::HasMetadata g=>[MDWrapped f g a]->MdwMap f g a
mdwMapFromList mdws = M.fromList $ zip ((fromJust . conName . getMetadata) <$> mdws) mdws


type GBuilderTopC f g a = (BuildableC f g, GenericSOPC a, AllF (All (Builder f g)) (Code a))

instance GBuilderTopC f g a=>GBuilder f g a where
  gBuildM mdh ma = case ma of
    Nothing -> internalSum $ buildBlanks mdh
    Just x  -> let cn = fromJust (getConstructorName x) in internalSum $ snd . unzip . M.toList $ M.insert  cn (buildDefaulted mdh x) (buildBlankMap mdh)

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


type GBuilderC2 f g xss = (BuildableC f g, All2 (Builder f g) xss, SListI2 xss)
buildBlanks'::forall f g xss.GBuilderC2 f g xss => g->DatatypeName->NP ConstructorInfo xss->[(FABuildable f) (SOP I xss)]
buildBlanks' mdh tn cs =
  let allBuilder2 = Proxy :: Proxy (All (Builder f g))
      pop = POP $ hcliftA allBuilder2 (buildBlank mdh tn) cs -- POP f xss
      wrapped = hliftA wrapBuildable pop -- POP (FABuildable f) xss
      sop = apInjs_POP wrapped -- [SOP (FABuildable f) xss]
      in hsequence <$> sop -- [(FABuildable f) (SOP I xss)]


type GBuilderC1 f g xs  = (BuildableC f g, All (Builder f g) xs,  SListI xs)
buildBlank::forall f g xs.GBuilderC1 f g xs => g->DatatypeName->ConstructorInfo xs->NP f xs
buildBlank mdh tn ci = 
    let mdhBase = setMetadata (Metadata tn (Just $ ci2name ci) Nothing) mdh
        fieldNames = ci2RecordNames ci
        allBuilder = Proxy :: Proxy (Builder f g)
    in case fieldNames of 
         Nothing -> hcpure allBuilder (buildM mdhBase Nothing)
         Just fns -> 
             let builder::Builder f g a=>FieldInfo a -> f a
                 builder fi = buildM (addFieldName fi mdhBase) Nothing
             in hcliftA allBuilder builder fns


buildDefaulted::forall f g a.GBuilderTopC f g a => g->a->MDWrapped f g a
buildDefaulted mdh a =
  let  {- mConName = getConstructorName a -}
      allBuilder2 = Proxy :: Proxy (All (Builder f g))
      (tn,cs) = case datatypeInfo (Proxy :: Proxy a) of
        ADT _ tn cs -> (tn,cs)
        Newtype _ tn c -> (tn,(c :* Nil))
--      baseMdh = setMetadata (Metadata tn mConName (fieldName . getMetadata $ mdh)) mdh
      sopf   = SOP $ hcliftA2 allBuilder2 (buildDefFromConInfo mdh tn) cs (unSOP $ from a) -- SOP f xss
      sopFAf = hliftA wrapBuildable sopf                                                   -- SOP (FABuilder f) xss
      fa = unFA $ (fmap to) . hsequence $ sopFAf                                           -- f a
  in MDWrapped True mdh fa


buildDefFromConInfo::forall f g xs.GBuilderC1 f g xs=>g->DatatypeName->ConstructorInfo xs->NP I xs->NP f xs
buildDefFromConInfo mdh tn ci args =
  let mdhBase = setMetadata (Metadata tn (Just $ ci2name ci) Nothing) mdh
      fieldNames = ci2RecordNames ci
  in case fieldNames of
      Nothing -> hcliftA (Proxy :: Proxy (Builder f g)) (\ia -> buildM mdhBase (Just (unI ia))) args
      Just fns ->
        let builder::Builder f g a=>FieldInfo a->I a->f a
            builder fi ia = buildM (addFieldName fi mdhBase) (Just (unI ia))
        in hcliftA2 (Proxy :: Proxy (Builder f g)) builder fns args

getConstructorName::forall a.GenericSOPC a=>a->Maybe ConName
getConstructorName a =
  let cs = case datatypeInfo (Proxy :: Proxy a) of
        ADT _ tn cs -> cs
        Newtype _ tn c -> c :* Nil
      getConName::ConstructorInfo xs->NP I xs->K (Maybe ConName) xs
      getConName ci args = K $ Just (ci2name ci)
  in hcollapse $ hliftA2 getConName cs (unSOP $ from a)

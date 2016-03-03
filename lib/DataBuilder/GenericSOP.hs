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
--{-# LANGUAGE PolyKinds #-}
--{-# LANGUAGE DeriveDataTypeable #-}
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

module DataBuilder.GenericSOP where

import qualified GHC.Generics as GHC
import Generics.SOP
import Generics.SOP.Constraint (SListIN(..))
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



gBuildMD::forall g a.(HasMetadata g,Generic a, HasDatatypeInfo a)=>g->a->[g]
gBuildMD mdh _ = case datatypeInfo (Proxy :: Proxy a) of
  ADT _ typeName cs    -> gAddMD mdh typeName cs
  Newtype _ typeName c -> gAddMD mdh typeName (c :* Nil)

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
type GBuilderC1 f g xs = (Buildable f g,HasMetadata g,All (Builder f g) xs,SListI xs)
type GBuilderC2 f g xss = (Buildable f g,HasMetadata g,All2 (Builder f g) xss, SListI2 xss)


--newMetadata::HasMetadata g=>g->DatatypeName->ConstructorInfo xs->g->g
--newMetadata tn ci mdh = setMetadata (Metadata tn (Just $ ci2name ci) (fieldName . getMetadata mdh))

addFieldName::forall a g.HasMetadata g=>FieldInfo a->g->g
addFieldName fi mdh = let (FieldInfo fName) = fi in setMetadata ((getMetadata mdh)  { fieldName = Just fName }) mdh

-- SListN can prob be SListI ?
buildBlanks::forall f g a xss.(GBuilderC2 f g xss, GenericSOPC a, xss ~ Code a)=>g->[MDWrapped f g a]
buildBlanks mdh = 
    let dtInfo = datatypeInfo (Proxy :: Proxy a)
        (tn,cs) = case dtInfo of
          ADT _ tn cs -> (tn,cs)
          Newtype _ tn c -> (tn,(c :* Nil))
        mdhs = gAddMD mdh tn cs
        builders = (unFA . fmap to) <$> buildBlanks' mdh tn cs
        mbs = zip mdhs builders
        makeMDW (mdh,bldr) = MDWrapped False mdh bldr
    in makeMDW <$> mbs


buildBlanks'::forall f g xss.(GBuilderC2 f g xss)=>g->DatatypeName->NP ConstructorInfo xss->[(FABuildable f) (SOP I xss)]
buildBlanks' mdh tn cs =
  let allBuilder::Proxy (All (Builder f g))
      allBuilder = Proxy
      pop = POP $ hcliftA allBuilder (buildBlank mdh tn) cs -- POP f xss
      wrapped = hliftA wrapBuildable pop -- POP (FABuildable f) xss
      sop = apInjs_POP wrapped -- [SOP (FABuildable f) xss]
      in hsequence <$> sop -- [(FABuildable f) (SOP I xss)]

buildBlank::forall f g xs.GBuilderC1 f g xs=>g->DatatypeName->ConstructorInfo xs->NP f xs
buildBlank mdh tn ci = 
    let mdhBase = setMetadata (Metadata tn (Just $ ci2name ci) Nothing) mdh
        fieldNames = ci2RecordNames ci
    in case fieldNames of 
         Nothing -> hcpure (Proxy :: Proxy (Builder f g)) (buildM mdhBase Nothing)
         Just fns -> 
             let builder::Builder f g a=>FieldInfo a -> f a
                 builder fi = buildM (addFieldName fi mdhBase) Nothing
             in hcliftA (Proxy :: Proxy (Builder f g)) builder fns

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

ci2md::HasMetadata g=>g->DatatypeName->ConstructorInfo xs->K g xs
ci2md mdh tn ci = K $ setMetadata (Metadata tn (Just $ ci2name ci) (fieldName . getMetadata $ mdh)) mdh

gAddMD::(HasMetadata g,SListI2 xss)=>g->DatatypeName->NP ConstructorInfo xss ->[g]
gAddMD mdh tn cs = hcollapse $ hliftA (ci2md mdh tn) cs


type GBuilderC f g a = (Buildable f g, Builder f g a, Generic a, HasDatatypeInfo a)
{-
gBuildBlankAs::forall f g a.GBuilderC f g a=>g->a->[f a]
gBuildBlankAs mdh _ = case datatypeInfo (Proxy :: Proxy a) of
  ADT _ typeName cs    -> gBuildBlanks mdh typeName cs
  Newtype _ typeName c -> gBuildBlanks mdh typeName (c :* Nil)

-}

--allBuilder :: Proxy (All (Builder f g))
--allBuilder = Proxy


gBuildBlank::forall f g a h xs.(HasMetadata g, GBuilderC f g a,HPure h, AllN h (Builder f g) xs, SListIN h xs)=>g->DatatypeName->ConstructorInfo xs->h f xs
gBuildBlank mdh tn ci = hcpure (Proxy :: Proxy (Builder f g)) (buildM mdh Nothing)

--gBuildBlanks::forall f g a.(GBuilder f g a)=>g->DataTypeName->NP ContructorInfo xss->[f a]


{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}

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
--import Generics.SOP.Sing (SListI(..))
import DataBuilder.InternalTypes
type GBuilderC a = (Generic a, HasDatatypeInfo a)

data Test  =  A Int String | B Char Char | C Char Bool deriving (GHC.Generic) 

instance Generic Test
instance HasDatatypeInfo Test

gBuildMD::forall a.GBuilderC a=>a->[Metadata]
gBuildMD _ = case datatypeInfo (Proxy :: Proxy a) of
  ADT _ typeName cs    -> gMakeMD typeName cs
  Newtype _ typeName c -> gMakeMD typeName (c :* Nil)

ci2name::ConstructorInfo xs-> ConName
ci2name (Constructor cn) = cn
ci2name (Infix cn _ _) = cn
ci2name (Record cn _) = cn

ci2md::DatatypeName->ConstructorInfo xs->K Metadata xs
ci2md tn ci = K $ Metadata tn (Just $ ci2name ci) Nothing

gMakeMD::SListI2 xss=>DatatypeName->NP ConstructorInfo xss -> [Metadata]
gMakeMD tn cs = hcollapse $ hliftA (ci2md tn) cs


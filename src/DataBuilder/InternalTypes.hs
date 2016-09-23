{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
-----------------------------------------------------------------------------
--
-- Module      :  DataBuilder.InternalTypes
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
module DataBuilder.InternalTypes
  (
    FieldName
  , ConName
  , MDWrapped(..)
  , FValidation(..)
  , Buildable(..)
  , Builder(..)
  , GBuilder(..)
  , buildAFromConList
    -- * Not exposed outside the library
  , internalSum
  ) where

import Data.Maybe (isJust)
import Data.Validation (Validation(..))
import Data.Semigroup (Semigroup,(<>))

import qualified Generics.SOP as GSOP

type FieldName = String
type ConName = String

newtype FValidation f err a = FValidation { unFValidation::f (Validation err a) }

instance Functor f => Functor (FValidation f err) where
  fmap h = FValidation . fmap (fmap h) . unFValidation

applyValidation::Semigroup err=>Validation err (a->b)->Validation err a->Validation err b
applyValidation (Success f) (Success a) = Success $ f a
applyValidation (Success f) (Failure e) = Failure e
applyValidation (Failure e) (Success a) = Failure e
applyValidation (Failure e1) (Failure e2) = Failure $ e1 <> e2

instance (Semigroup err, Applicative f) => Applicative (FValidation f err) where
  pure = FValidation . pure . pure
  fvh <*> fva = FValidation $ fmap applyValidation (unFValidation fvh) <*> (unFValidation fva) 

data MDWrapped f err a = MDWrapped { hasDefault::Bool, metadata::(ConName,Maybe FieldName), value::FValidation f err a }

class (Applicative f,Semigroup err)=>Buildable f err where
  bFail::String->FValidation f err a -- if there's a graceful way to handle errors...
  bSum::[MDWrapped f err a]->FValidation f err a -- used to decide how to represent a sum.  E.g., chooser in an HTML form

class (Buildable f err, GSOP.Generic a, GSOP.HasDatatypeInfo a) => GBuilder f err a where
  gBuildA::Maybe FieldName->Maybe a-> FValidation f err a

class Buildable f err=>Builder f err a where
  buildA::Maybe FieldName->Maybe a-> FValidation f err a
  default buildA::GBuilder f err a=>Maybe FieldName->Maybe a-> FValidation f err a
  buildA = gBuildA

buildAFromConList::Buildable f err=>[(Maybe FieldName->Maybe a->MDWrapped f err a)]->Maybe FieldName->Maybe a->FValidation f err a
buildAFromConList conList mFN ma = internalSum $ fmap (\f->f mFN ma) conList

internalSum::Buildable f err=>[MDWrapped f err a]->FValidation f err a
internalSum mdws = case length mdws of
  0 -> bFail "Internal error in DataBuilder.  No Constructors in Sum!"
  1 -> value (head mdws)
  _ -> bSum mdws

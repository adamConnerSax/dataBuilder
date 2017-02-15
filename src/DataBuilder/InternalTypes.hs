{-# LANGUAGE CPP                     #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
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
  , Buildable(..)
  , Builder(..)
  , GBuilder(..)
  , buildAFromConList
    -- * Not exposed outside the library
  , internalSum
  ) where

import           Data.Maybe   (isJust)
import qualified Generics.SOP as GSOP
import Data.Validation (AccValidation)

type FieldName = String
type ConName = String
type Validator e a = a -> AccValidation e a

data MDWrapped f e a = MDWrapped { hasDefault::Bool, metadata::(ConName,Maybe FieldName), value::f (AccValidation e a) }

class Applicative f=>Buildable f e where
  noConsError::e
  bFail::e->f (AccValidation e a) -- if there's a graceful way to handle errors...
  bSum::[MDWrapped f e a]->f (AccValidation e a) -- used to decide how to represent a sum.  E.g., chooser in an HTML form

class (GSOP.Generic a, GSOP.HasDatatypeInfo a) => GBuilder f e a where
  gBuildA::Buildable f e=>Validator e a->Maybe FieldName->Maybe a->f (AccValidation e a)

class Builder f e a where
  validateA::Validator e a
  validateA = Right
  buildA::Buildable f e=>Maybe FieldName->Maybe a-> f (AccValidation e a)
  default buildA::(Buildable f e, GBuilder f e a)=>Maybe FieldName->Maybe a->f (Validation e a)
  buildA = gBuildA validateA

-- ??
buildAFromConList::Buildable f e=>[(Validator e a->Maybe FieldName->Maybe a->MDWrapped f e a)]->Validator e a->Maybe FieldName->Maybe a->f (Validation e a)
buildAFromConList conList va mFN ma  = internalSum noConsError $ fmap (\f->f va mFN ma) conList

internalSum::Buildable f e=>[MDWrapped f e a]->f (AccValidation e a)
internalSum mdws = case length mdws of
  0 -> bFail noConsError
  1 -> value (head mdws)
  _ -> bSum mdws

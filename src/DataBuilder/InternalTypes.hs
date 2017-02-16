{-# LANGUAGE CPP                     #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
--{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE FunctionalDependencies  #-}
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
  , Validator
  , VF(..)
  , Buildable(..)
  , Builder(..)
  , GBuilder(..)
  , buildAFromConList
    -- * Not exposed outside the library
  , mergeAV
  , internalSum
  ) where

import           Data.Maybe   (isJust)
import qualified Generics.SOP as GSOP
import Data.Validation (AccValidation(..))
import Data.Semigroup (Semigroup)


type FieldName = String
type ConName = String
type Validator e a = a -> AccValidation e a

newtype VF f e a = VF { unVF::f (AccValidation e a) }
instance Functor f => Functor (VF f e) where
  fmap g vf = VF $ (fmap g) <$> unVF vf

instance (Applicative f,Semigroup e)=>Applicative (VF f e) where
  pure = VF . pure . AccSuccess
  vfg <*> vfa = VF $ (fmap (<*>) (unVF vfg)) <*> (unVF vfa)  

mergeAV::AccValidation e (AccValidation e a)->AccValidation e a
mergeAV x = case x of
  AccFailure err -> AccFailure err
  AccSuccess x -> x
  
data MDWrapped f e a = MDWrapped { hasDefault::Bool, metadata::(ConName,Maybe FieldName), value::VF f e a }

class Applicative f=>Buildable f e | e->f where
  noConsError::e
  bFail::e->VF f e a -- if there's a graceful way to handle errors...
  bSum::[MDWrapped f e a]->VF f e a -- used to decide how to represent a sum.  E.g., chooser in an HTML form

class (GSOP.Generic a, GSOP.HasDatatypeInfo a) => GBuilder f e a where
  gBuildA::Buildable f e=>Validator e a->Maybe FieldName->Maybe a->VF f e a

class Builder f e a | e->f where
  validateA::Validator e a
  validateA = AccSuccess
  buildA::Buildable f e=>Maybe FieldName->Maybe a-> VF f e a
  default buildA::(Buildable f e, GBuilder f e a)=>Maybe FieldName->Maybe a->VF f e a
  buildA = gBuildA validateA

-- ??
buildAFromConList::Buildable f e=>[(Validator e a->Maybe FieldName->Maybe a->MDWrapped f e a)]->Validator e a->Maybe FieldName->Maybe a->VF f e a
buildAFromConList conList va mFN ma  = internalSum $ fmap (\q->q va mFN ma) conList

internalSum::Buildable f e=>[MDWrapped f e a]->VF f e a
internalSum mdws = case length mdws of
  0 -> bFail noConsError
  1 -> value (head mdws)
  _ -> bSum mdws

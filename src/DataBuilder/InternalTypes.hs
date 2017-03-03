{-# LANGUAGE CPP                     #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RankNTypes              #-}
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
  , Validatable(..)
  , FV(..)
  , makeFV
  , unFV
  , fToFV
  , Buildable(..)
  , Builder(..)
  , buildA
  , GBuilder(..)
  , buildAFromConList
  , validateFV
  , validatefv
  , MonadLike(..)
    -- * Not exposed outside the library
  , internalSum
  ) where

import           Control.Applicative  (Alternative (..))
import           Control.Monad        (join)
import           Data.Functor.Compose (Compose (..))
import           Data.Maybe           (isJust)
import           Data.Semigroup       (Semigroup)
import           Data.Validation      (AccValidation (..))
import qualified Generics.SOP         as GSOP

type FieldName = String
type ConName = String
type Validator v a = a -> v a

type FV f v = Compose f v

makeFV::f (v a) -> FV f v a
makeFV = Compose

unFV::FV f v a -> f (v a)
unFV = getCompose

fToFV::(Functor f, MonadLike v)=>f a -> FV f v a
fToFV = makeFV . fmap pureLike


{- NB: these look monadish but v may not be a monad but have reasonable definitions of these, e.g., AccValidate -}
class MonadLike f where
  pureLike::a -> f a
  default pureLike::Applicative f=>a -> f a
  pureLike = pure
  joinLike::f (f a) -> f a
  default joinLike::Monad f=>f (f a) -> f a
  joinLike = join

validatefv::(Functor f,Functor v,MonadLike v)=>Validator v a ->f (v a) -> f (v a)
validatefv va = fmap joinLike . getCompose . fmap va . Compose

validateFV::(Functor f,Functor v,MonadLike v)=>Validator v a ->FV f v a -> FV f v a
validateFV va = Compose . (fmap joinLike) . getCompose . fmap va

data MDWrapped f v a = MDWrapped { hasDefault::Bool, metadata::(ConName,Maybe FieldName), value::FV f v a }

class (Applicative f, Applicative v)=>Buildable f v  where
  bFail::String->FV f v a -- if there's a graceful way to handle errors...
  bSum::[MDWrapped f v a]->FV f v a -- used to decide how to represent a sum.  E.g., chooser in an HTML form

class (GSOP.Generic a, GSOP.HasDatatypeInfo a) => GBuilder f v a where
  gBuildValidated::Buildable f v=>Validator v a->Maybe FieldName->Maybe a->FV f v a

-- this is meant to be overriden for anything with any kind of validation
-- and needs to be instantiated for any type being built
class Validatable v a where
  validate::Validator v a
  default validate::MonadLike v=>Validator v a
  validate = pureLike

class Buildable f v => Builder f v a  where
  buildValidated::Buildable f v=>Validator v a->Maybe FieldName->Maybe a-> FV f v a
  default buildValidated::(Buildable f v, GBuilder f v a)=>Validator v a->Maybe FieldName->Maybe a->FV f v a
  buildValidated va = gBuildValidated va

buildA::(Builder f v a,MonadLike v,Validatable v a)=>Maybe FieldName->Maybe a-> FV f v a
buildA = buildValidated validate

buildAFromConList::Buildable f v=>[(Validator v a->Maybe FieldName->Maybe a->MDWrapped f v a)]->Validator v a->Maybe FieldName->Maybe a->FV f v a
buildAFromConList conList va mFN ma  = internalSum $ fmap (\q->q va mFN ma) conList

internalSum::Buildable f v=>[MDWrapped f v a]->FV f v a
internalSum mdws = case length mdws of
  0 -> bFail "No Constructors in sum (this shouldn't happen!)."
  1 -> value (head mdws)
  _ -> bSum mdws

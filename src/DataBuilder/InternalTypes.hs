{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}

{-|
Module: DataBuilder.InternalTypes
Description: Types and TypeClasses for DataBuilder package
Copyright: (c) Adam Conner-Sax 2016
License: BSD3
Maintainer: adam_conner_sax@yahoo.com

This module contains basic types and typeclasses for the package.
-}

module DataBuilder.InternalTypes
  (
    FieldName
  , ConName
  , MDWrapped(..)
  , Buildable(..)
  , Builder(..)
  , GBuilder(..)
    -- * Not exposed outside the library
  , internalSum
--  , wrapBuildable
--  , FABuildable(unFA)
  ) where

import Data.Maybe (isJust)
import qualified Generics.SOP as GSOP

type FieldName = String
type ConName = String

data MDWrapped f a = MDWrapped { hasDefault::Bool, metadata::(ConName,Maybe FieldName), value::f a }

{-|
We don't get the Functor and applicative methods from those classes becuase we may want to use this
in a case where the underlying f is not Functor or Applicative.  E.g., Reflex.Dynamic.  Though it needs to have equivalent
functionality.
-}
class Applicative f=>Buildable f where
  bFail::String->f a -- if there's a graceful way to handle errors...
  bSum::[MDWrapped f a]->f a -- used to decide how to represent a sum.  E.g., chooser in an HTML form

class (GSOP.Generic a, GSOP.HasDatatypeInfo a) => GBuilder f a where
  gBuildA::Buildable f=>Maybe FieldName->Maybe a-> f a

class Builder f a where
  buildA::Buildable f=>Maybe FieldName->Maybe a-> f a
  default buildA::(Buildable f, GBuilder f a)=>Maybe FieldName->Maybe a-> f a
  buildA = gBuildA

internalSum::Buildable f=>[MDWrapped f a]->f a
internalSum mdws = case length mdws of
  0 -> bFail "Internal error in DataBuilder.  No Constructors in Sum!"
  1 -> value (head mdws)
  _ -> bSum mdws

{-
newtype FABuildable f a = FABuildable { unFA::f a }

-- we don't expose the constructor but we do expose wrapBuildable.  That way we can only wrap if f is Buildable
wrapBuildable::Buildable f=>f a->FABuildable f a
wrapBuildable = FABuildable

instance Buildable f=>Functor (FABuildable f) where
  fmap f x = FABuildable $ (bMap f) (unFA x)

instance Buildable f=>Applicative (FABuildable f) where
  pure x = FABuildable $ bInject x
  x <*> y = FABuildable $ (unFA x) `bApply` (unFA y)
-}

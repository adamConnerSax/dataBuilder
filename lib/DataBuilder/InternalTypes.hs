{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
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
    TypeName
  , FieldName
  , ConName
  , Metadata(..)
  , typeOnlyMD
  , HasMetadata(..)
  , HasMetadataFields(..)
  , MDWrapped(..)
  , Buildable(..)
  , Builder(..)
    -- * Not exposed outside the library
  , internalSum
  , wrapBuildable
  , FABuildable(unFA)
  , GBuilder(..)
  ) where

import Data.Maybe (isJust)
import qualified Generics.SOP as GSOP

type TypeName = String
type FieldName = String
type ConName = String

data Metadata = Metadata { typeName::TypeName, conName::Maybe ConName, fieldName::Maybe FieldName} deriving (Show)

typeOnlyMD::TypeName->Metadata
typeOnlyMD tn = Metadata tn Nothing Nothing

{-| We could use lenses for all this but that brings a lot of dependencies in where we had few. So we do it manually here. -}
class HasMetadata a where
  getMetadata::a->Metadata
  setMetadata::Metadata->a->a

class HasMetadataFields a where
  getTypeName::a->TypeName
  setTypeName::a->TypeName->a
  getmConName::a->Maybe ConName
  setConName::a->ConName->a
  getmFieldName::a->Maybe FieldName
  setFieldName::a->FieldName->a

instance HasMetadata Metadata where
  getMetadata = id
  setMetadata x _ = x

instance HasMetadata a => HasMetadataFields a where
  getTypeName = typeName . getMetadata
  setTypeName mdh tn = setMetadata ((getMetadata mdh) { typeName = tn }) mdh
  getmConName = conName . getMetadata
  setConName mdh cn = setMetadata ((getMetadata mdh) { conName = Just cn }) mdh 
  getmFieldName = fieldName . getMetadata
  setFieldName mdh fn = setMetadata ((getMetadata mdh) { fieldName = Just fn }) mdh 


data MDWrapped f g a = MDWrapped { hasDefault::Bool, metadataHolder::g, value::f a }

instance HasMetadata g=>HasMetadata (MDWrapped f g a) where
  getMetadata = getMetadata . metadataHolder
  setMetadata md (MDWrapped hd mdh v) = let mdh' = setMetadata md mdh in MDWrapped hd mdh' v

mdwHasConName::HasMetadata g=>MDWrapped f g a->Bool
mdwHasConName mdw = isJust (getmConName mdw)

buildersAllHaveConNames::HasMetadata g=>[MDWrapped f g a]->Bool
buildersAllHaveConNames bes = not (any (not . mdwHasConName) bes)

{-|
We don't get the Functor and applicative methods from those classes becuase we may want to use this
in a case where the underlying f is not Functor or Applicative.  E.g., Reflex.Dynamic.  Though it needs to have equivalent
functionality.
-}
class Buildable f g | f->g where
  -- so we can derive the functor instance of the wrapped version
  bMap::(a->b) -> f a->f b
  -- inject and apply are exactly Applicative, inject=pure and apply=(<*>).
  bInject::a -> f a
  bApply::f (a->b) -> f a -> f b
  bFail::String->f a -- if there's a graceful way to handle errors...
  bSum::[MDWrapped f g a]->f a -- used to decide how to represent a sum.  E.g., chooser in an HTML form

class (GSOP.Generic a, GSOP.HasDatatypeInfo a) => GBuilder f g a where
  gBuildM::(HasMetadata g,Buildable f g)=>g->Maybe a-> f a

class Builder f g a where
  buildM::(HasMetadata g,Buildable f g)=>g->Maybe a-> f a
  default buildM::(HasMetadata g, Buildable f g, GBuilder f g a)=>g->Maybe a-> f a
  buildM = gBuildM

internalSum::(HasMetadata g, Buildable f g)=>[MDWrapped f g a]->f a
internalSum mdws = case length mdws of
  0 -> bFail "Internal error in DataBuilder.  No Constructors in Sum!"
  1 -> value (head mdws)
  _ -> if buildersAllHaveConNames mdws then bSum mdws else bFail "Sum type encountered but constructor name(s) are missing."


newtype FABuildable f a = FABuildable { unFA::f a }

-- we don't expose the constructor but we do expose wrapBuildable.  That way we can only wrap if f is Buildable
wrapBuildable::Buildable f g=>f a->FABuildable f a
wrapBuildable = FABuildable

instance Buildable f g=>Functor (FABuildable f) where
  fmap f x = FABuildable $ (bMap f) (unFA x)

instance Buildable f g=>Applicative (FABuildable f) where
  pure x = FABuildable $ bInject x
  x <*> y = FABuildable $ (unFA x) `bApply` (unFA y)

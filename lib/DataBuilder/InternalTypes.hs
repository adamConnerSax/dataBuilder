{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
module DataBuilder.InternalTypes
  (
    TypeName
  , FieldName
  , ConName
  , Metadata(..)
  , typeOnlyMD
  , HasMetadata(..)
  , MDWrapped(..)
  , mdwConName
  , Buildable(..)
  , Builder(..)
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

class HasMetadata a where
  getMetadata::a->Metadata
  setMetadata::Metadata->a->a

instance HasMetadata Metadata where
  getMetadata = id
  setMetadata x _ = x

data MDWrapped f g a = MDWrapped { hasDefault::Bool, metadataHolder::g, value::f a }

instance HasMetadata g=>HasMetadata (MDWrapped f g a) where
  getMetadata = getMetadata . metadataHolder
  setMetadata md (MDWrapped hd mdh v) = let mdh' = setMetadata md mdh in MDWrapped hd mdh' v

mdwConName::HasMetadata g=>MDWrapped f g a -> Maybe ConName
mdwConName x = conName (getMetadata x)

mdwHasConName::HasMetadata g=>MDWrapped f g a->Bool
mdwHasConName mdw = isJust (mdwConName mdw)

buildersAllHaveConNames::HasMetadata g=>[MDWrapped f g a]->Bool
buildersAllHaveConNames bes = not (any (not . mdwHasConName) bes)

{- We don't get the Functor and applicative methods from those classes becuase we may want to use this
 - in a case where the underlying f is not Functor or Applicative.  E.g., Reflex.Dynamic
-}
class Buildable f g | f->g where
  -- here only so we can derive the functor instance of the wrapped version
  bMap::(a->b) -> f a->f b
  -- inject and apply are exactly Applicative, inject=pure and apply=(<*>).
  bInject::a -> f a
  bApply::f (a->b) -> f a -> f b
  bFail::String->f a -- if there's a graceful way to handle errors...
  bSum::[MDWrapped f g a]->f a -- this only gets called if you have > 1 and all have constructor names in Metadata

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
  _ -> if buildersAllHaveConNames mdws then bSum mdws else bFail "Sum type for command encountered but constructor name(s) are missing."

-- This is available to make clear that this structure is inherent in Buildable and so it can be used if necessary
newtype FABuildable f a = FABuildable { unFA::f a }

-- we don't expose the constructor but we do expose wrapBuildable.  That way we can only wrap if f is Buildable
wrapBuildable::Buildable f g=>f a->FABuildable f a
wrapBuildable = FABuildable

instance Buildable f g=>Functor (FABuildable f) where
  fmap f x = FABuildable $ (bMap f) (unFA x)

instance Buildable f g=>Applicative (FABuildable f) where
  pure x = FABuildable $ bInject x
  x <*> y = FABuildable $ (unFA x) `bApply` (unFA y)

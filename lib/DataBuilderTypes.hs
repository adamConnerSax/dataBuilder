{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module DataBuilderTypes
  (
    TypeName
  , FieldName
  , ConName
  , Metadata(typeName,conName,fieldName)
  , typeOnlyMD
  , HasMetadata(..)
  , MDWrapped(hasDefault,metadataHolder,value)
  , mdwConName
  , Buildable(..)
  , Builder(..)
  , internalSum
  )where

type TypeName = String
type FieldName = String
type ConName = String

data Metadata = Metadata { typeName::TypeName, conName::Maybe ConName, fieldName::Maybe FieldName}

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
mdwHasConName mdw = maybe False (const True) (mdwConName mdw)

buildersAllHaveConNames::HasMetadata g=>[MDWrapped f g a]->Bool
buildersAllHaveConNames bes = null (filter (not . mdwHasConName) bes)


class Buildable f g | f->g where
  -- inject and apply are exactly Applicative, inject=pure and apply=(<*>).
  bInject::a -> f a
  bApply::f (a->b) -> f a -> f b
  bFail::String->f a -- if there's a graceful way to handle errors...
  bSum::[MDWrapped f g a]->f a -- this only gets called if you have > 1 and all have constructor names in Metadata

class Builder f g a where
  buildM::(HasMetadata g,Buildable f g)=>g->Maybe a-> f a

internalSum::(HasMetadata g, Buildable f g)=>[MDWrapped f g a]->f a
internalSum mdws = case (length mdws) of
  0 -> bFail "Internal error in DataBuilder.  No Constructors in Sum!"
  1 -> value (head mdws)
  _ -> if (buildersAllHaveConNames mdws) then bSum mdws else bFail "Sum type for command encountered but constructor name(s) are missing."

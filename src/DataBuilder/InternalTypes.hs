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

buildAFromConList::Buildable f=>[(Maybe FieldName->Maybe a->MDWrapped f a)]->Maybe FieldName->Maybe a->f a
buildAFromConList conList mFN ma = internalSum $ fmap (\f->f mFN ma) conList

internalSum::Buildable f=>[MDWrapped f a]->f a
internalSum mdws = case length mdws of
  0 -> bFail "Internal error in DataBuilder.  No Constructors in Sum!"
  1 -> value (head mdws)
  _ -> bSum mdws

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
  , FGV(..)
  {-
  , makeFV
  , unFV
  , fToFV
-}
  , Buildable(..)
  , Builder(..)
  , buildA
  , GBuilder(..)
  , buildAFromConList
  {-
  , validateFV
  , validatefv
-}
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

{-
type FV f v = Compose f v

makeFV::f (v a) -> FV f v a
makeFV = Compose

unFV::FV f v a -> f (v a)
unFV = getCompose

fToFV::(Functor f, MonadLike v)=>f a -> FV f v a
fToFV = makeFV . fmap pureLike
-}

{- NB: these look monadish but f may not be a monad but have reasonable definitions of these, e.g., AccValidate -}
class MonadLike f where
  pureLike::a -> f a
  default pureLike::Applicative f=>a -> f a
  pureLike = pure
  joinLike::f (f a) -> f a
  default joinLike::Monad f=>f (f a) -> f a
  joinLike = join


validate::(Functor g, Functor v, MonadLike v)=>Validator v a->g (v a)->g (v a)
validate f = fmap (joinLike . fmap f) 

fmapComposed::(Functor f, Functor g)=>(a->b) -> f (g a)-> f (g b)
fmapComposed f = getCompose . fmap f . Compose

{-
validatefv::(Functor f,Functor v,MonadLike v)=>Validator v a ->f (v a) -> f (v a)
validatefv va = fmap joinLike . getCompose . fmap va . Compose

validateFV::(Functor f,Functor v,MonadLike v)=>Validator v a ->FV f v a -> FV f v a
validateFV va = Compose . (fmap joinLike) . getCompose . fmap va
-}

newtype FGV f g v a = FGV { unFGV::f (g (v a)) }

comp2 = Compose . Compose
gcomp2 = getCompose . getCompose

instance (Functor f, Functor g, Functor v)=>Functor (FGV f g v) where
  fmap f = FGV . gcomp2 . fmap f . comp2 . unFGV

instance (Applicative f, Applicative g, Applicative v)=>Applicative (FGV f g v) where
  pure = FGV . gcomp2 . pure 
  fgvF <*> fgvA = FGV $ gcomp2  (comp2 (unFGV fgvF) <*> comp2 (unFGV fgvA))

data MDWrapped f g v a = MDWrapped { hasDefault::Bool, metadata::(g ConName,Maybe FieldName), value::FGV f g v a }

class (Applicative f, Applicative g, Applicative v)=>Buildable f g v  where
  bFail::String->FGV f g v a -- if there's a graceful way to handle errors...
  bSum::[MDWrapped f g v a]->FGV f g v a -- used to decide how to represent a sum.  E.g., chooser in an HTML form

class (GSOP.Generic a, GSOP.HasDatatypeInfo a) => GBuilder f g v a where
  gBuildValidated::Buildable f g v=>Validator v a->Maybe FieldName->Maybe (g a)->FGV f g v a

-- this is meant to be overriden for anything with any kind of validation
-- and needs to be instantiated for any type being built
class Validatable v a where
  validator::Validator v a
  default validator::MonadLike v=>Validator v a
  validator = pureLike

class Buildable f g v => Builder f g v a  where
  buildValidated::Buildable f g v=>Validator v a->Maybe FieldName->Maybe (g a)-> FGV f g v a
  default buildValidated::(Buildable f g v, GBuilder f g v a)=>Validator v a->Maybe FieldName->Maybe (g a)->FGV f g v a
  buildValidated va = gBuildValidated va

buildA::(Builder f g v a,MonadLike v,Validatable v a)=>Maybe FieldName->Maybe (g a)-> FGV f g v a
buildA = buildValidated validator

buildAFromConList::Buildable f g v=>[(Validator v a->Maybe FieldName->Maybe (g a)->MDWrapped f g v a)]->Validator v a->Maybe FieldName->Maybe (g a)->FGV f g v a
buildAFromConList conList va mFN mga  = internalSum $ fmap (\q->q va mFN mga) conList

internalSum::Buildable f g v=>[MDWrapped f g v a]->FGV f g v a
internalSum mdws = case length mdws of
  0 -> bFail "No Constructors in sum (this shouldn't happen!)."
  1 -> value (head mdws)
  _ -> bSum mdws

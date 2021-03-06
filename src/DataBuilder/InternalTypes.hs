{-# LANGUAGE CPP                     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE TypeOperators           #-}
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
  , makeMDWrapped
  , SimpleMDWrapped(..)
  , Validator
  , Validatable(..)
  , GV(..)
  , FGV(..)
  , fToFGV
  , Buildable(..)
  , SimpleBuildable(..)
  , Builder(..)
  , SimpleBuilder(..)
  , buildA
  , GBuilder(..)
  , CustomSequenceG
  , GBuilderCS(..)
  , SimpleGBuilder(..)
--  , buildAFromConList
  , validate
  , validateFGV
  , MonadLike(..)
  , MaybeLike(..)

    -- * Not exposed outside the library
--  , internalSum
  , internalSum
  , internalSumSimple
  ) where

import           Control.Applicative   (Alternative (..))
import           Control.Monad         (join)
import           Data.Functor.Compose  (Compose (..))
import           Data.Functor.Identity (Identity)
import           Data.Maybe            (fromMaybe, isJust)
import           Data.Semigroup        (Semigroup)
import           Generics.SOP          ((:.:), NP, SListI)
import qualified Generics.SOP          as GSOP
type FieldName = String
type ConName = String
type Validator v a = a -> v a


{- NB: these look monadish but f may not be a monad but have reasonable definitions of these, e.g., AccValidate -}
class MonadLike f where
  pureLike :: a -> f a
  default pureLike::Applicative f=>a -> f a
  pureLike = pure
  joinLike :: f (f a) -> f a
  default joinLike::Monad f=>f (f a) -> f a
  joinLike = join

instance MonadLike Identity

class MaybeLike v where
  absorbMaybe :: v (Maybe a) -> v a
  toMaybe :: v a -> Maybe a

instance MaybeLike Maybe where
  absorbMaybe = join
  toMaybe = id

validate :: (Functor g, Functor v, MonadLike v)=>Validator v a->g (v a)->g (v a)
validate f = fmap (joinLike . fmap f)

fmapComposed :: (Functor f, Functor g)=>(a->b) -> f (g a)-> f (g b)
fmapComposed f = getCompose . fmap f . Compose

newtype GV g v a = GV { unGV::g (v a) }
gToGV :: (Functor g, MonadLike g, MonadLike v)=>g a -> GV g v a
gToGV = GV . fmap pureLike

validateGV :: (Functor g, Functor v, MonadLike v)=>Validator v a -> GV g v a -> GV g v a
validateGV  valA = GV . validate valA . unGV

gvToComposed = Compose . unGV
composedToGV = GV . getCompose

instance (Functor g, Functor v)=>Functor (GV g v) where
  fmap f = composedToGV . fmap f . gvToComposed

instance (Applicative g, Applicative v) => Applicative (GV g v) where
  pure = composedToGV . pure
  gvF <*> gvA = composedToGV (gvToComposed gvF <*> gvToComposed gvA)

instance (Alternative g, Applicative g, Applicative v) => Alternative (GV g v) where
  empty = composedToGV empty
  gvA <|> gvB = composedToGV $ gvToComposed gvA <|> gvToComposed gvB

newtype FGV f g v a = FGV { unFGV::f (g (v a)) }

fToFGV :: (Functor f, MonadLike v, MonadLike g)=>f a -> FGV f g v a
fToFGV = FGV . fmap (pureLike . pureLike)

validateFGV :: (Functor f, Functor g, Functor v, MonadLike v) => Validator v a -> FGV f g v a -> FGV f g v a
validateFGV valA = FGV . fmap (validate valA) . unFGV

fgvToComposed = Compose . Compose . unFGV
composedToFGV = FGV . getCompose . getCompose

instance (Functor f, Functor g, Functor v)=>Functor (FGV f g v) where
  fmap f = composedToFGV . fmap f . fgvToComposed

instance (Applicative f, Applicative g, Applicative v)=>Applicative (FGV f g v) where
  pure = composedToFGV . pure
  fgvF <*> fgvA = composedToFGV  (fgvToComposed fgvF <*> fgvToComposed fgvA)

instance (Alternative f, Alternative g, Applicative f, Applicative g, Applicative v)=>Alternative (FGV f g v) where
  empty = composedToFGV empty
  fgvA <|> fgvB = composedToFGV $ fgvToComposed fgvA <|> fgvToComposed fgvB

data MDWrapped f g v a = MDWrapped { hasDefault :: g Bool
                                   , metadata::(ConName, Maybe FieldName)
                                   , value :: FGV f g v a
                                   }
makeMDWrapped :: (Functor v, MaybeLike v, Functor g) => Maybe FieldName -> (a -> Bool) -> (GV g v a -> FGV f g v a) -> ConName -> GV g v a -> MDWrapped f g v a
makeMDWrapped mFN isThis valueThis name gva =
  let isThisGV = fmap (fromMaybe False . toMaybe) . unGV . fmap isThis
  in MDWrapped (isThisGV gva) (name, mFN) (valueThis gva)

class (Applicative f, Applicative g, Applicative v) => Buildable f g v  where
  bFail :: String->FGV f g v a -- if there's a graceful way to handle errors...
  bSum :: [MDWrapped f g v a]->FGV f g v a -- used to decide how to represent a sum.  E.g., chooser in an HTML form

class (Buildable f g v, GSOP.Generic a, GSOP.HasDatatypeInfo a) => GBuilder r f g v a where
  gBuildValidated :: r -> Validator v a -> Maybe FieldName -> GV g v a -> FGV f g v a

type CustomSequenceG g = (forall (xs :: [*]) (j :: * -> *). SListI xs => (NP (g :.: j) xs) -> g (NP j xs))

-- For the case when g has a specific way to sequence rather than using its applicative instance
class (Buildable f g v, GSOP.Generic a, GSOP.HasDatatypeInfo a) => GBuilderCS r f g v a where
  gBuildValidatedCS :: r -> CustomSequenceG g -> Validator v a -> Maybe FieldName -> GV g v a -> FGV f g v a

-- this is meant to be overriden for anything with any kind of validation
-- and needs to be instantiated for any type being built
class Validatable v a where
  validator::Validator v a
  default validator::MonadLike v=>Validator v a
  validator = pureLike

class Buildable f g v => Builder r f g v a  where
  buildValidated :: Buildable f g v => r -> Validator v a -> Maybe FieldName -> GV g v a -> FGV f g v a
  default buildValidated :: (Buildable f g v, GBuilder r f g v a) => r -> Validator v a -> Maybe FieldName -> GV g v a -> FGV f g v a
  buildValidated = gBuildValidated

buildA :: (Builder r f g v a, Validatable v a) => r -> Maybe FieldName -> GV g v a -> FGV f g v a
buildA r = buildValidated r validator

--buildAFromConList::Buildable f g v=>[Validator v a->Maybe FieldName->GV g v a->MDWrapped f g v a]->Validator v a->Maybe FieldName->GV g v a->FGV f g v a
--buildAFromConList conList va mFN mga  = internalSum' $ fmap (\q->q va mFN mga) conList

--internalSum::Buildable f g v=>g [MDWrapped f g v a]->FGV f g v a
--internalSum = bCollapse . fmap internalSum'

internalSum :: Buildable f g v => [MDWrapped f g v a] -> FGV f g v a
internalSum mdws = case length mdws of
  0 -> bFail "No Constructors in sum (this shouldn't happen!)."
  1 -> value (head mdws)
  _ -> bSum mdws

data SimpleMDWrapped f a = SimpleMDWrapped { simple_hasDefault :: Bool
                                           , simple_metadata :: (ConName, Maybe FieldName)
                                           , simple_value :: f a
                                           }

class Applicative f => SimpleBuildable f where
  simpleBFail :: String -> f a
  simpleBSum :: [SimpleMDWrapped f a] -> f a

-- r is a tag type, used only to allow different sets of instances.

class (GSOP.Generic a, GSOP.HasDatatypeInfo a) => SimpleGBuilder r f a where
  gSimpleBuild :: SimpleBuildable f => r -> Maybe FieldName -> Maybe a -> f a

class SimpleBuildable f => SimpleBuilder r f a where
  simpleBuild :: r -> Maybe FieldName -> Maybe a -> f a
  default simpleBuild :: SimpleGBuilder r f a => r -> Maybe FieldName -> Maybe a -> f a
  simpleBuild = gSimpleBuild

internalSumSimple :: SimpleBuildable f => [SimpleMDWrapped f a] -> f a
internalSumSimple smdws = case length smdws of
  0 -> simpleBFail "No Constructors in sum (this shouldn't happen!)."
  1 -> simple_value (head smdws)
  _ -> simpleBSum smdws


{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE RankNTypes            #-}
-----------------------------------------------------------------------------
--
-- Module      :  DataBuilder.GenericSOP
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

module DataBuilder.GenericSOP
       (
         module GSOP --re-export the Generics.SOP classes for deriving
       , module Generics.SOP.TH
       , All
       , All2
       , And
       , Code
       , buildMDWrappedList
       )where


import           Control.Monad (join)
import           Data.Maybe                (isJust,fromMaybe)
import           Data.Semigroup            (Semigroup)
import           Generics.SOP              hiding (FieldName, constructorName)
import           Generics.SOP              as GSOP (Generic, HasDatatypeInfo)
import           Generics.SOP.TH           (deriveGeneric)
import qualified GHC.Generics              as GHC

import           Generics.SOP.PerConstructor

import           DataBuilder.InternalTypes



type VBuilderC f g v = And (Builder f g v) (Validatable v)
type GBuilderC f g v a = (Buildable f g v, Generic a, HasDatatypeInfo a, All2 (VBuilderC f g v) (Code a))

mFaSGBuilder::forall f g v a.(Generic a,HasDatatypeInfo a
                             , Applicative f
                             , Applicative g
                             , Applicative v
                             , All2 (VBuilderC f g v) (Code a),MaybeLike v)
  =>Proxy a -> MapFieldsAndSequence (GV g v :.: Maybe) (FGV f g v) (Code a) -- POP GV xss ->  NP (FGV :.: NP I) xss
mFaSGBuilder {- proxy popGVM -} = mFaSGBuilderCustomSequence hsequence

-- special case when g, our original functor has an optimized sequence
applyCustomSequenceG::(Applicative f, Applicative v, Functor g, SListI xs)
  =>(forall (xs :: [*]) (j :: * -> *).SListI xs=>(NP (g :.: j) xs -> g (NP j xs))) -> NP (FGV f g v) xs -> FGV f g v (NP I xs) 
applyCustomSequenceG cs = FGV . fmap (fmap hsequence) . fmap cs . hsequence' . hmap (Comp . fmap Comp . unFGV)  


mFaSGBuilderCustomSequence::forall f g v a.(Generic a,HasDatatypeInfo a
                             , Applicative f
                             , Functor g -- Applicative g
                             , Applicative v
                             , All2 (VBuilderC f g v) (Code a),MaybeLike v)
  =>(forall (xs::[*]).SListI xs=>NP (FGV f g v) xs -> FGV f g v (NP I xs))--(forall (xs :: [*])  (h :: * -> *) .SListI xs=>NP (g :.: h) xs -> g (NP h xs))
  ->Proxy a
  ->MapFieldsAndSequence (GV g v :.: Maybe) (FGV f g v) (Code a) -- POP GV xss ->  NP (FGV :.: NP I) xss
mFaSGBuilderCustomSequence customSequence proxy popGVM =
  let vbuilderC = Proxy :: Proxy (VBuilderC f g v)
      sListIC = Proxy :: Proxy SListI
      fixGVM::Functor g=>(GV g v :.: Maybe) x -> GV g v x
      fixGVM = GV . fmap absorbMaybe . unGV . unComp
  in hcliftA sListIC (Comp . customSequence ) . unPOP $ hcliftA2 vbuilderC (\kmfn gvma -> buildA (unK kmfn) (fixGVM gvma)) (maybeFieldNamePOP proxy) popGVM


buildMDWrappedList::forall f g v a.(MonadLike v, MaybeLike v, GBuilderC f g v a)
  => Maybe FieldName
  -> GV g v a
  -> [MDWrapped f g v a]
buildMDWrappedList mf gva = 
  let proxyA = Proxy :: Proxy a
      conNameList        = constructorNameList proxyA
      origAndWidgetList  = functorDoPerConstructor' (mFaSGBuilder proxyA) gva
      fixOrig = fmap (fromMaybe False . toMaybe) . unGV . fmap isJust
  in zipWith (\name (orig,widget) -> MDWrapped (fixOrig orig) (name,mf) widget) conNameList origAndWidgetList

instance (MonadLike v, MaybeLike v, GBuilderC f g v a)=>GBuilder f g v a where
  gBuildValidated valA mf gva = validateFGV valA . internalSum $ buildMDWrappedList mf gva 


buildMDWrappedListCS::forall f g v a.(MonadLike v, MaybeLike v, GBuilderC f g v a)
  => CustomSequenceG g
  -> Maybe FieldName
  -> GV g v a
  -> [MDWrapped f g v a]
buildMDWrappedListCS cs mf gva = 
  let proxyA = Proxy :: Proxy a
      conNameList        = constructorNameList proxyA
      origAndWidgetList  = functorDoPerConstructor' (mFaSGBuilderCustomSequence (applyCustomSequenceG cs) proxyA) gva
      fixOrig = fmap (fromMaybe False . toMaybe) . unGV . fmap isJust
  in zipWith (\name (orig,widget) -> MDWrapped (fixOrig orig) (name,mf) widget) conNameList origAndWidgetList

instance (MonadLike v, MaybeLike v, GBuilderC f g v a)=>GBuilderCS f g v a where
  gBuildValidatedCS cs valA mf gva = validateFGV valA . internalSum $ buildMDWrappedListCS cs mf gva 


type SimpleGBuilderC f a = (SimpleBuildable f, Generic a, HasDatatypeInfo a, All2 (SimpleBuilder f) (Code a))

mFaSGSimpleBuilder::forall f a.(Generic a,HasDatatypeInfo a
                               , Applicative f
                               , All2 (SimpleBuilder f) (Code a))
                  =>Proxy a -> MapFieldsAndSequence (Maybe :.: Maybe) f (Code a) -- POP (Maybe :.: I) xss ->  NP (f :.: NP I) xss
mFaSGSimpleBuilder proxy popM =
  let builderC = Proxy :: Proxy (SimpleBuilder f)
      sListIC = Proxy :: Proxy SListI
      fix::(Maybe :.: Maybe) x -> Maybe x
      fix = join . unComp
  in hcliftA sListIC (Comp . hsequence) . unPOP $ hcliftA2 builderC (\kmfn ima -> simpleBuild (unK kmfn) (fix ima)) (maybeFieldNamePOP proxy) popM
  
instance SimpleGBuilderC f a=>SimpleGBuilder f a where
  gSimpleBuild mf ma = 
    let proxyA             = Proxy :: Proxy a
        conNameList        = constructorNameList proxyA
        origAndWidgetList  = functorDoPerConstructor' (mFaSGSimpleBuilder proxyA) ma
        fixOrig            = isJust . join
    in internalSumSimple $ zipWith (\name (orig,widget) -> SimpleMDWrapped (fixOrig orig) (name,mf) widget) conNameList origAndWidgetList




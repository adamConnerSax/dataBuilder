{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
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


import           Control.Monad               (join)
import           Data.Maybe                  (fromMaybe, isJust)
import           Data.Semigroup              (Semigroup)
import           Generics.SOP                hiding (FieldName, constructorName)
import           Generics.SOP                as GSOP (Generic, HasDatatypeInfo)
import           Generics.SOP.TH             (deriveGeneric)
import qualified GHC.Generics                as GHC

import           Generics.SOP.PerConstructor

import           DataBuilder.InternalTypes



type VBuilderC r f g v = And (Builder r f g v) (Validatable v)
type GBuilderC r f g v a = (Buildable f g v, Generic a, HasDatatypeInfo a, All2 (VBuilderC r f g v) (Code a))

mFaSGBuilder :: forall r f g v a. ( Generic a
                                  , HasDatatypeInfo a
                                  , Applicative f
                                  , Applicative g
                                  , Applicative v
                                  , All2 (VBuilderC r f g v) (Code a)
                                  , MaybeLike v)
  => r -> Proxy a -> MapFieldsAndSequence (GV g v :.: Maybe) (FGV f g v) (Code a) -- POP GV xss ->  NP (FGV :.: NP I) xss
mFaSGBuilder r = mFaSGBuilderCustomSequence r hsequence

-- special case when g, our original functor has an optimized sequence
applyCustomSequenceG :: ( Applicative f
                        , Applicative v
                        , Functor g
                        , SListI xs)
  =>(forall (xs :: [*]) (j :: * -> *) . SListI xs=>(NP (g :.: j) xs -> g (NP j xs))) -> NP (FGV f g v) xs -> FGV f g v (NP I xs)
applyCustomSequenceG cs = FGV . fmap (fmap hsequence . cs) . hsequence' . hmap (Comp . fmap Comp . unFGV)

mFaSGBuilderCustomSequence :: forall r f g v a. ( Generic a
                                                , HasDatatypeInfo a
                                                , Applicative f
                                                , Functor g -- Applicative g
                                                , Applicative v
                                                , All2 (VBuilderC r f g v) (Code a)
                                                , MaybeLike v)
  => r
  -> (forall (xs :: [*]). SListI xs => NP (FGV f g v) xs -> FGV f g v (NP I xs))
  -> Proxy a
  -> MapFieldsAndSequence (GV g v :.: Maybe) (FGV f g v) (Code a) -- POP GV xss ->  NP (FGV :.: NP I) xss
mFaSGBuilderCustomSequence r customSequence proxy popGVM =
  let vbuilderC = Proxy :: Proxy (VBuilderC r f g v)
      sListIC = Proxy :: Proxy SListI
      fixGVM :: Functor g => (GV g v :.: Maybe) x -> GV g v x
      fixGVM = GV . fmap absorbMaybe . unGV . unComp
  in hcliftA sListIC (Comp . customSequence ) . unPOP $ hcliftA2 vbuilderC (\kmfn gvma -> buildA r (unK kmfn) (fixGVM gvma)) (maybeFieldNamePOP proxy) popGVM


buildMDWrappedList :: forall r f g v a. (MonadLike v, MaybeLike v, GBuilderC r f g v a)
  => r
  -> Maybe FieldName
  -> GV g v a
  -> [MDWrapped f g v a]
buildMDWrappedList r mf gva =
  let proxyA = Proxy :: Proxy a
      conNameList        = constructorNameList proxyA
      origAndWidgetList  = functorDoPerConstructor' (mFaSGBuilder r proxyA) gva
      fixOrig = fmap (fromMaybe False . toMaybe) . unGV . fmap isJust
  in zipWith (\name (orig,widget) -> MDWrapped (fixOrig orig) (name,mf) widget) conNameList origAndWidgetList

instance (MonadLike v, MaybeLike v, GBuilderC r f g v a) => GBuilder r f g v a where
  gBuildValidated r valA mf gva = validateFGV valA . internalSum $ buildMDWrappedList r mf gva

buildMDWrappedListCS :: forall r f g v a. (MonadLike v, MaybeLike v, GBuilderC r f g v a)
  => r
  -> CustomSequenceG g
  -> Maybe FieldName
  -> GV g v a
  -> [MDWrapped f g v a]
buildMDWrappedListCS r cs mf gva =
  let proxyA = Proxy :: Proxy a
      conNameList        = constructorNameList proxyA
      origAndWidgetList  = functorDoPerConstructor' (mFaSGBuilderCustomSequence r (applyCustomSequenceG cs) proxyA) gva
      fixOrig = fmap (fromMaybe False . toMaybe) . unGV . fmap isJust
  in zipWith (\name (orig,widget) -> MDWrapped (fixOrig orig) (name,mf) widget) conNameList origAndWidgetList

instance (MonadLike v, MaybeLike v, GBuilderC r f g v a) => GBuilderCS r f g v a where
  gBuildValidatedCS r cs valA mf gva = validateFGV valA . internalSum $ buildMDWrappedListCS r cs mf gva

type SimpleGBuilderC r f a = (SimpleBuildable f, Generic a, HasDatatypeInfo a, All2 (SimpleBuilder r f) (Code a))

mFaSGSimpleBuilder :: forall r f a. ( Generic a
                                    , HasDatatypeInfo a
                                    , Applicative f
                                    , All2 (SimpleBuilder r f) (Code a))
  => r -> Proxy a -> MapFieldsAndSequence (Maybe :.: Maybe) f (Code a) -- POP (Maybe :.: I) xss ->  NP (f :.: NP I) xss
mFaSGSimpleBuilder r proxy popM =
  let builderC = Proxy :: Proxy (SimpleBuilder r f)
      sListIC = Proxy :: Proxy SListI
      fix :: (Maybe :.: Maybe) x -> Maybe x
      fix = join . unComp
  in hcliftA sListIC (Comp . hsequence) . unPOP $ hcliftA2 builderC (\kmfn ima -> simpleBuild r (unK kmfn) (fix ima)) (maybeFieldNamePOP proxy) popM

instance SimpleGBuilderC r f a => SimpleGBuilder r f a where
  gSimpleBuild r mf ma =
    let proxyA             = Proxy :: Proxy a
        conNameList        = constructorNameList proxyA
        origAndWidgetList  = functorDoPerConstructor' (mFaSGSimpleBuilder r proxyA) ma
        fixOrig            = isJust . join
    in internalSumSimple $ zipWith (\name (orig,widget) -> SimpleMDWrapped (fixOrig orig) (name,mf) widget) conNameList origAndWidgetList




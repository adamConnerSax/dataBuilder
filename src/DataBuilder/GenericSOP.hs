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
       )where


import           Control.Monad (join)
import qualified Data.Map                  as M
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
mFaSGBuilder proxy popGVM =
  let vbuilderC = Proxy :: Proxy (VBuilderC f g v)
      sListIC = Proxy :: Proxy SListI
      fixGVM::Functor g=>(GV g v :.: Maybe) x -> GV g v x
      fixGVM = GV . fmap absorbMaybe . unGV . unComp
  in hcliftA sListIC (Comp . hsequence) . unPOP $ hcliftA2 vbuilderC (\kmfn gvma -> buildA (unK kmfn) (fixGVM gvma)) (maybeFieldNamePOP proxy) popGVM
  
instance (MonadLike v, MaybeLike v, GBuilderC f g v a)=>GBuilder f g v a where
  gBuildValidated valA mf gva = 
    let proxyA = Proxy :: Proxy a
        conNameList        = constructorNameList proxyA
        origAndWidgetList  = functorDoPerConstructor' (mFaSGBuilder proxyA) gva
        fixOrig = fmap (fromMaybe False . toMaybe) . unGV . fmap isJust
    in internalSum' $ zipWith (\name (orig,widget) -> MDWrapped (fixOrig orig) (name,mf) widget) conNameList origAndWidgetList


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



--
{-
ci2name::ConstructorInfo xs-> ConName
ci2name (Constructor cn) = cn
ci2name (Infix cn _ _) = cn
ci2name (Record cn _) = cn

ci2RecordNames::ConstructorInfo xs -> Maybe (NP FieldInfo xs)
ci2RecordNames (Record _ fi) = Just fi
ci2RecordNames _ = Nothing


ci2md::Maybe FieldName->DatatypeName->ConstructorInfo xs->K (ConName,Maybe FieldName) xs
ci2md mf tn ci = K $ (ci2name ci,mf)


gMakeMDs::SListI2 xss=>Maybe FieldName->DatatypeName->NP ConstructorInfo xss ->[(ConName,Maybe FieldName)]
gMakeMDs mf tn cs = hcollapse $ hliftA (ci2md mf tn) cs

type GenericSOPC a = (Generic a, HasDatatypeInfo a)

fi2mf::FieldInfo a->Maybe FieldName
fi2mf (FieldInfo x) = Just x

type MdwMap f g v a = M.Map ConName (MDWrapped f g v a)

mdwMapToList::MdwMap f g v a->[MDWrapped f g v a]
mdwMapToList = snd . unzip . M.toList

mdwMapFromList::[MDWrapped f g v a]->MdwMap f g v a
mdwMapFromList mdws = M.fromList $ zip ((fst . metadata) <$> mdws) mdws

type GBuilderTopC f g v a = (Buildable f g v, GenericSOPC a, All2 (VBuilderC f g v) (Code a))

instance (MonadLike v, GBuilderTopC f g v a)=>GBuilder f g v a where
  gBuildValidated va mf mga = case mga of
    Nothing -> internalSum' $ buildBlanks va mf
    Just gx  ->
      let insertMDW m mdw = M.insert (fst . metadata $ mdw) mdw m
      in bCollapseAndSum $ snd . unzip . M.toList . insertMDW (buildBlankMap va mf) <$> (buildDefaulted va mf gx)

buildBlankMap::forall f g v a.(MonadLike v, GBuilderTopC f g v a) => Validator v a->Maybe FieldName->MdwMap f g v a
buildBlankMap va = mdwMapFromList . buildBlanks va


buildBlanks::forall f g v a.(MonadLike v, GBuilderTopC f g v a) =>Validator v a->Maybe FieldName->[MDWrapped f g v a]
buildBlanks va mf =
    let (tn,cs) = case datatypeInfo (Proxy :: Proxy a) of
          ADT _ tn cs -> (tn,cs)
          Newtype _ tn c -> (tn,(c :* Nil))
        mds = gMakeMDs mf tn cs
        builders = FGV . fmap (validate va) . unFGV  . fmap to <$> buildBlanks' mf tn cs -- here's where we validate the constructed type (1)
        mbs = zip mds builders
        makeMDW (md',bldr) = MDWrapped False md' bldr
    in makeMDW <$> mbs

--type All2C f xss = (All2 (Builder f) xss)
type VBuilderC f g v = And (Builder f g v) (Validatable v)
type GBuilderC2 f g v xss = (Buildable f g v, All2 (VBuilderC f g v) xss, SListI2 xss)
buildBlanks'::forall f g v xss.(MonadLike v, GBuilderC2 f g v xss) => Maybe FieldName->DatatypeName->NP ConstructorInfo xss->[FGV f g v (SOP I xss)]
buildBlanks' mf tn cs =
  let allBuilder = Proxy :: Proxy (All (VBuilderC f g v))
      pop = POP $ hcliftA allBuilder (buildBlank mf tn) cs
      sop = apInjs_POP pop -- [SOP f xss]
  in hsequence <$> sop -- [f (SOP I xss)]

--type GBuilderC3 f v  = (Builder f v, Validatable v)
type GBuilderC1 f g v xs  = (Buildable f g v, All (VBuilderC f g v) xs, SListI xs)
buildBlank::forall f g v xs.(MonadLike v,GBuilderC1 f g v xs) => Maybe FieldName->DatatypeName->ConstructorInfo xs->NP (FGV f g v) xs
buildBlank mf tn ci =
    let fieldNames = ci2RecordNames ci
        builderC = Proxy :: Proxy (And (Builder f g v) (Validatable v))
    in case fieldNames of
           Nothing -> hcpure builderC (buildA Nothing Nothing)
           Just fns ->
             let builder::(Validatable v a,Builder f g v a)=>FieldInfo a -> FGV f g v a
                 builder fi = buildA (fi2mf fi) Nothing
             in hcliftA builderC builder fns


{-
buildDefaultedDS::forall f g v a.(MonadLike v, GBuilderTopC f g v a) => Validator v a->Maybe FieldName->g a->g (DS.DSum (ConTag a) g)
buildDefaultedDS va mf ga =
-}

buildDefaulted::forall f g v a.(MonadLike v, GBuilderTopC f g v a) => Validator v a->Maybe FieldName->g a->g (MDWrapped f g v a)
buildDefaulted va mf = fmap (buildDefaulted' va mf)

buildDefaulted'::forall f g v a.(MonadLike v, GBuilderTopC f g v a) => Validator v a->Maybe FieldName->a->MDWrapped f g v a
buildDefaulted' va mf a =
  let cn = constructorName' a
      allBuilder = Proxy :: Proxy (All (VBuilderC f g v))
      (tn,cs) = case datatypeInfo (Proxy :: Proxy a) of
        ADT _ tn cs -> (tn,cs)
        Newtype _ tn c -> (tn,(c :* Nil))
      sopf   = SOP $ hcliftA2 allBuilder (buildDefFromConInfo mf tn) cs (unSOP $ from a) -- SOP (FGV f g v) xss
      vfa = validateFGV va . fmap to . hsequence $ sopf   -- here's where we validate the constructed type (2)
  in MDWrapped True (cn,mf) vfa


buildDefFromConInfo::forall f g v xs.(MonadLike v, GBuilderC1 f g v xs)
  =>Maybe FieldName
  ->DatatypeName
  ->ConstructorInfo xs -- constructor info for each field
  ->NP I xs -- value in each field as an NP I
  ->NP (FGV f g v) xs -- built value in each field as an NP (FGV f g v)
buildDefFromConInfo md tn ci args =
  let fieldNames = ci2RecordNames ci
      builderC = Proxy :: Proxy (VBuilderC f g v)
  in case fieldNames of
      Nothing ->
        let builder::(MonadLike v, Builder f g v a,Validatable v a)=>I a -> FGV f g v a
            builder a = buildA Nothing (Just $ pure (unI a))
        in hcliftA builderC builder args
      Just fns ->
        let builder::(MonadLike v, Builder f g v a, Validatable v a)=>FieldInfo a->I a->FGV f g v a
            builder fi a = buildA (fi2mf fi) (Just $ pure (unI a))
        in hcliftA2 builderC builder fns args


constructorName::forall a g.(Functor g, GenericSOPC a)=>g a->g ConName
constructorName = fmap constructorName'

constructorName'::forall a.GenericSOPC a=>a->ConName
constructorName' a =
  let cs = case datatypeInfo (Proxy :: Proxy a) of
        ADT _ tn cs -> cs
        Newtype _ tn c -> c :* Nil
      getConName::ConstructorInfo xs->NP I xs->K ConName xs
      getConName ci _ = K $ ci2name ci
  in hcollapse $ hliftA2 getConName cs (unSOP $ from a)
-}

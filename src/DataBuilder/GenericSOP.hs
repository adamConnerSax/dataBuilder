{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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


import qualified Data.Map                  as M
import           Data.Maybe                (fromJust)
import           Data.Semigroup            (Semigroup)
import           Generics.SOP              hiding (FieldName, constructorName)
import           Generics.SOP              as GSOP (Generic, HasDatatypeInfo)
import           Generics.SOP.TH           (deriveGeneric)
import qualified GHC.Generics              as GHC

--
import           DataBuilder.InternalTypes

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
    Nothing -> internalSum $ buildBlanks va mf
    Just gx  -> let cn = (constructorName x) in internalSum . snd . unzip . M.toList $ M.insert cn (buildDefaulted va mf gx) (buildBlankMap va mf)

buildBlankMap::forall f g v a.(MonadLike v, GBuilderTopC f g v a) => Validator v a->Maybe FieldName->MdwMap f g v a
buildBlankMap va = mdwMapFromList . buildBlanks va


buildBlanks::forall f g v a.(MonadLike v, GBuilderTopC f g v a) =>Validator v a->Maybe FieldName->[MDWrapped f g v a]
buildBlanks va mf =
    let (tn,cs) = case datatypeInfo (Proxy :: Proxy a) of
          ADT _ tn cs -> (tn,cs)
          Newtype _ tn c -> (tn,(c :* Nil))
        mds = gMakeMDs mf tn cs
        builders = (fmap (fmap  (validate va . fmap to))) <$> buildBlanks' mf tn cs -- here's where we validate the constructed type (1)
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

buildDefaulted::forall f g v a.(MonadLike v, GBuilderTopC f g v a) => Validator v a->Maybe FieldName->g a->MDWrapped f g v a
buildDefaulted va mf ga =
  let gcn = constructorName a
      allBuilder = Proxy :: Proxy (All (VBuilderC f g v))
      (tn,cs) = case datatypeInfo (Proxy :: Proxy a) of
        ADT _ tn cs -> (tn,cs)
        Newtype _ tn c -> (tn,(c :* Nil))
      sopf   = SOP $ hcliftA2 allBuilder (buildDefFromConInfo mf tn) cs (unSOP . from <$> ga) -- SOP (VF f e) xss
      vfa = validateFV va . fmap to . hsequence $ sopf   -- here's where we validate the constructed type (2)
  in MDWrapped True (gcn,mf) vfa


buildDefFromConInfo::forall f g v xs.(MonadLike v, GBuilderC1 f g v xs)=>Maybe FieldName->DatatypeName->ConstructorInfo xs->NP g xs->NP (FGV f g v) xs
buildDefFromConInfo md tn ci args =
  let fieldNames = ci2RecordNames ci
      builderC = Proxy :: Proxy (VBuilderC f v)
  in case fieldNames of
      Nothing ->
        let builder::(MonadLike v, Builder f g v a,Validatable v a)=>g a -> FGV f g v a
            builder ga = buildA Nothing (Just ga)
        in hcliftA builderC builder args
      Just fns ->
        let builder::(MonadLike v, Builder f v a, Validatable v a)=>FieldInfo a->g a->FGV f g v a
            builder fi ga = buildA (fi2mf fi) (Just ga)
        in hcliftA2 builderC builder fns args


constructorName::forall a g.(Functor g, GenericSOPC a)=>g a->g ConName
constructorName = fmap constructorName'

constructorName'::forall a.GenericSOPC a=>a->ConName
constructorName' a =
  let cs = case datatypeInfo (Proxy :: Proxy a) of
        ADT _ tn cs -> cs
        Newtype _ tn c -> c :* Nil
      getConName::ConstructorInfo xs->NP I xs->K ConName xs
      getConName ci args = K $ ci2name ci
  in hcollapse $ hliftA2 getConName cs (unSOP $ from a)

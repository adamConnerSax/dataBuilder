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


import qualified Data.Map                  as M
import           Data.Maybe                (fromJust)
import           Data.Semigroup            (Semigroup)
import           Generics.SOP              hiding (FieldName, constructorName)
import           Generics.SOP              as GSOP (Generic, HasDatatypeInfo)
import           Generics.SOP.TH           (deriveGeneric)
import qualified GHC.Generics              as GHC

--import qualified GHC.TypeLits as TL
--import qualified Data.Singletons as S
import qualified Data.Dependent.Map        as DM
import qualified Data.Dependent.Sum        as DS
import           Data.GADT.Compare         ((:~:) (..), GCompare (..), GEq (..),
                                            GOrdering (..))
--
import           DataBuilder.InternalTypes


-- some preliminaries for type-level tags for the constructors of sum types
-- Can we just use NS or NP itself??
-- I don't think so, or at least we don't want to.  We need a type that encodes the set of constructors
-- I think we want to use NP ConstructorInfo xs


-- no good, I think, because the first parameter is changed by a call to There.
-- That is,
data NPTag (xs :: [k]) (x :: k) where -- x is in xs
  Here  :: NPTag (x ': xs) x          -- x begins xs
  There :: NPTag xs x -> NPTag (y ': xs) x -- given that x is in xs, x is also in (y ': xs)

npToDMap::NP f xs -> DM.DMap (NPTag xs) f
npToDMap Nil = DM.empty
npToDMap (fx :* np') = DM.insert Here fx $ DM.mapKeysMonotonic There $ npToDMap np'


instance GEq (NPTag xs) where
  geq Here      Here      = Just Refl
  geq (There x) (There y) = geq x y
  geq _         _         = Nothing


instance All2 (Compose Eq FieldInfo) xss=> DS.EqTag (NPTag xss) ConstructorInfo where
  eqTagged Here      Here      = (==)

instance GCompare (NPTag xs) where
  gcompare Here Here = GEQ
  gcompare Here (There _) = GLT
  gcompare (There _) Here = GGT
  gcompare (There x) (There y) = gcompare x y

instance (All2 (Compose Eq FieldInfo) xss
         ,All2 (Compose Ord FieldInfo) xss)=>DS.OrdTag (NPTag xss) ConstructorInfo where
  compareTagged Here Here = compare


ciDMap::forall a xss.(HasDatatypeInfo a, xss ~ Code a)=>a->DM.DMap (NPTag xss) ConstructorInfo
ciDMap a =
  let {- ciInfo :: NP ConstructorInfo (Code a) -}
      ciInfo = constructorInfo $ datatypeInfo (Proxy :: Proxy a) -- NP ConstructorInfo xss
  in npToDMap ciInfo


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

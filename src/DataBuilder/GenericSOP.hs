{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import qualified GHC.Generics as GHC
import Generics.SOP hiding (FieldName)
import Generics.SOP as GSOP (Generic,HasDatatypeInfo)
import Generics.SOP.Dict
import Generics.SOP.TH (deriveGeneric)
import qualified Data.Map as M
import Data.Maybe (fromJust)
--
import DataBuilder.InternalTypes

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

type MdwMap f a = M.Map ConName (MDWrapped f a)

mdwMapToList::MdwMap f a->[MDWrapped f a]
mdwMapToList = snd . unzip . M.toList

mdwMapFromList::[MDWrapped f a]->MdwMap f a
mdwMapFromList mdws = M.fromList $ zip ((fst . metadata) <$> mdws) mdws

type GBuilderTopC f a = (Buildable f, GenericSOPC a, All2 (Builder f) (Code a))

instance GBuilderTopC f a=>GBuilder f a where
  gBuildA mf ma = case ma of
    Nothing -> internalSum $ buildBlanks mf
    Just x  -> let cn = (constructorName x) in internalSum . snd . unzip . M.toList $ M.insert cn (buildDefaulted mf x) (buildBlankMap mf)

buildBlankMap::forall f a.GBuilderTopC f a => Maybe FieldName->MdwMap f a
buildBlankMap = mdwMapFromList . buildBlanks


buildBlanks::forall f a.GBuilderTopC f a => Maybe FieldName->[MDWrapped f a]
buildBlanks mf = 
    let (tn,cs) = case datatypeInfo (Proxy :: Proxy a) of
          ADT _ tn cs -> (tn,cs)
          Newtype _ tn c -> (tn,(c :* Nil))
        mds = gMakeMDs mf tn cs
        builders = fmap to <$> buildBlanks' mf tn cs
        mbs = zip mds builders
        makeMDW (md',bldr) = MDWrapped False md' bldr
    in makeMDW <$> mbs

--type All2C f xss = (All2 (Builder f) xss)
type GBuilderC2 f xss = (Buildable f, All2 (Builder f) xss, SListI2 xss)
buildBlanks'::forall f xss.GBuilderC2 f xss => Maybe FieldName->DatatypeName->NP ConstructorInfo xss->[f (SOP I xss)]
buildBlanks' mf tn cs =
  let allBuilder = Proxy :: Proxy (All (Builder f))
      pop = POP $ hcliftA allBuilder (buildBlank mf tn) cs
--      wrapped = hliftA wrapBuildable pop -- POP (FABuildable f) xss
      sop = apInjs_POP pop -- [SOP f xss]
  in hsequence <$> sop -- [f (SOP I xss)]


type GBuilderC1 f xs  = (Buildable f, All (Builder f) xs, SListI xs)
buildBlank::forall f xs.GBuilderC1 f xs => Maybe FieldName->DatatypeName->ConstructorInfo xs->NP f xs
buildBlank mf tn ci = 
    let fieldNames = ci2RecordNames ci
        builderC = Proxy :: Proxy (Builder f)
    in case fieldNames of 
           Nothing -> hcpure builderC (buildA Nothing Nothing)
           Just fns -> 
             let builder::Builder f a=>FieldInfo a -> f a
                 builder fi = buildA (fi2mf fi) Nothing
             in hcliftA builderC builder fns

buildDefaulted::forall f a.GBuilderTopC f a => Maybe FieldName->a->MDWrapped f a
buildDefaulted mf a =
  let cn = constructorName a
--      mdBase = setmConName mcn md
      allBuilder = Proxy :: Proxy (All (Builder f))
--      dHNP = unAll_NP $ unAll2 Dict :: NP (Dict (All HasDatatypeInfo)) (Code a)
      (tn,cs) = case datatypeInfo (Proxy :: Proxy a) of
        ADT _ tn cs -> (tn,cs)
        Newtype _ tn c -> (tn,(c :* Nil))
      sopf   = SOP $ hcliftA2 allBuilder (buildDefFromConInfo mf tn) cs (unSOP $ from a) -- SOP f xss
--      sopFAf = hliftA wrapBuildable sopf                                                   -- SOP (FABuilder f) xss
      fa = fmap to . hsequence $ sopf                                           -- f a
  in MDWrapped True (cn,mf) fa


buildDefFromConInfo::forall f xs.GBuilderC1 f xs=>Maybe FieldName->DatatypeName->ConstructorInfo xs->NP I xs->NP f xs
buildDefFromConInfo md tn ci args =
  let fieldNames = ci2RecordNames ci
      builderC = Proxy :: Proxy (Builder f)
  in case fieldNames of
      Nothing ->
        let builder::Builder f a=>I a -> f a
            builder ia = buildA Nothing (Just $ unI ia)
        in hcliftA builderC builder args
      Just fns ->
        let builder::Builder f a=>FieldInfo a->I a->f a
            builder fi ia = buildA (fi2mf fi) (Just (unI ia))
        in hcliftA2 builderC builder fns args

constructorName::forall a.GenericSOPC a=>a->ConName
constructorName a =
  let cs = case datatypeInfo (Proxy :: Proxy a) of
        ADT _ tn cs -> cs
        Newtype _ tn c -> c :* Nil
      getConName::ConstructorInfo xs->NP I xs->K ConName xs
      getConName ci args = K $ ci2name ci
  in hcollapse $ hliftA2 getConName cs (unSOP $ from a)

{-
gTypeName::forall a.HasDatatypeInfo a=>K DatatypeName a
gTypeName = case datatypeInfo (Proxy :: Proxy a) of
  ADT _ tn _ -> K tn
  Newtype _ tn _ -> K tn

gTypeNames::forall xs.(All HasDatatypeInfo xs, SListI xs)=>NP (K DatatypeName) xs
gTypeNames = hcpure (Proxy :: Proxy HasDatatypeInfo) gTypeName 
-}

{- Generic and HasDatatypeInfo instances for a bunch of basic types -}
instance Generic Int
instance HasDatatypeInfo Int

instance Generic Float
instance HasDatatypeInfo Float

instance Generic Double
instance HasDatatypeInfo Double

--instance Generic String
--instance HasDatatypeInfo String

instance Generic Char
instance HasDatatypeInfo Char


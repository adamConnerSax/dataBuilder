{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
module DataBuilder.OptionParser
       (
         makeOAParser
       , ParserBuilder(..)
       , Generic
       , HasDatatypeInfo
       ) where

import           Data.Char              (toLower)
import           Data.Maybe             (fromJust)
import           Data.Monoid            ((<>))
import           DataBuilder.GenericSOP ()
import           DataBuilder.Types
import           Options.Applicative

import           Data.Text              (Text)
type MDWrappedOA a = SimpleMDWrapped Parser a

type OABuilderC a = SimpleBuilder Parser a

class ParserBuilder a where
  buildParser::Maybe FieldName->Maybe a->Parser a
  default buildParser::SimpleGBuilder Parser a=>Maybe FieldName->Maybe a->Parser a
  buildParser = gSimpleBuild

instance ParserBuilder a=>SimpleBuilder Parser a where
  simpleBuild = buildParser

makeOAParser::OABuilderC a=>Maybe a->Parser a
makeOAParser = simpleBuild Nothing

instance SimpleBuildable Parser where
  simpleBFail msg = abortOption (ErrorMsg msg) mempty <*> option disabled mempty
  simpleBSum = sumToCommand


-- derive a command parser from a sum-type
sumToCommand::[MDWrappedOA a]->Parser a
sumToCommand mdws =
  let makeCommand mdw = command (fst . simple_metadata $ mdw) (info (simple_value mdw) mempty)
  in subparser $ mconcat (map makeCommand mdws)

--shortAndLong::HasName f=>String->Mod f a
shortAndLong x = long x <> short (head x)

parseReadable::(Read a,Show a)=>ReadM a->Maybe String->Maybe FieldName->Maybe a->Parser a
parseReadable reader mHelp mf ma =
  case mf of
    Nothing->argument reader (maybe mempty Options.Applicative.value ma <> maybe mempty help mHelp)
    Just fieldName -> option reader (maybe mempty Options.Applicative.value ma <> shortAndLong fieldName <> maybe mempty help mHelp)

instance ParserBuilder Int where
  buildParser = parseReadable auto (Just "Int")

instance ParserBuilder Double where
  buildParser = parseReadable auto (Just "Double")

instance ParserBuilder String where
  buildParser = parseReadable str (Just "String")

instance ParserBuilder Text where
  buildParser = parseReadable auto (Just "Text")


instance {-# OVERLAPPABLE #-} (Show e,Enum e,Bounded e)=>ParserBuilder e where
  buildParser mf mE = foldl (<|>) empty $ map (\ev->fl ev (optDesc <> (shortAndLong (toLower <$> show ev)))) [minBound :: e..] where
    fl = maybe flag' flag mE
    optDesc = maybe mempty help mf

instance {-# OVERLAPPABLE #-} ParserBuilder a=>ParserBuilder (Maybe a) where
  buildParser mf mmA = optional $ maybe (buildParser mf Nothing) (buildParser mf) mmA



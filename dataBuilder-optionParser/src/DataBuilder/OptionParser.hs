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
       , makeDefaultOAParser
       , enumAsExclusiveFlags
       , parseArgumentsToList
       , OADefault (OADefault)
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

import           Data.Text              (Text, unpack)
import           Data.Time              (TimeOfDay)
import           Data.Time.Calendar     (Day)
import           Data.Time.Format       (defaultTimeLocale, parseTimeM)

type MDWrappedOA a = SimpleMDWrapped Parser a

data OADefault = OADefault

type OABuilderC r a = (SimpleBuilder r Parser a)

class ParserBuilder r a where
  buildParser :: r -> Maybe FieldName -> Maybe a -> Parser a
  default buildParser :: SimpleGBuilder r Parser a => r -> Maybe FieldName -> Maybe a -> Parser a
  buildParser = gSimpleBuild

instance ParserBuilder r a => SimpleBuilder r Parser a where
  simpleBuild = buildParser

-- this allows you to create new instances for everything by creating instances for newtype wrapped versions.
makeOAParser :: OABuilderC r a => r -> Maybe a -> Parser a
makeOAParser r = simpleBuild r Nothing

makeDefaultOAParser :: OABuilderC OADefault a => Maybe a -> Parser a
makeDefaultOAParser = simpleBuild OADefault Nothing

instance SimpleBuildable Parser where
  simpleBFail msg = abortOption (ErrorMsg msg) mempty <*> option disabled mempty
  simpleBSum = sumToCommand

-- derive a command parser from a sum-type
sumToCommand :: [MDWrappedOA a] -> Parser a
sumToCommand mdws =
  let makeCommand mdw = command (fst . simple_metadata $ mdw) (info (simple_value mdw) mempty)
  in subparser $ mconcat (map makeCommand mdws)

--shortAndLong::HasName f=>String->Mod f a
shortAndLong x = long x <> short (head x)

-- TODO:  add mf to mHelp to make help text
parseReadable::(Read a, Show a) => ReadM a -> Maybe String -> Maybe FieldName -> Maybe a -> Parser a
parseReadable reader mHelp mf ma =
  case mf of
    Nothing -> argument reader (maybe mempty Options.Applicative.value ma <> maybe mempty help mHelp)
    Just fieldName -> option reader (maybe mempty Options.Applicative.value ma <> shortAndLong fieldName <> maybe mempty help mHelp)

instance ParserBuilder OADefault Bool where
  buildParser _ = enumAsExclusiveFlags

instance ParserBuilder OADefault Int where
  buildParser _ = parseReadable auto (Just "Int")

instance ParserBuilder OADefault Double where
  buildParser _ = parseReadable auto (Just "Double")

instance ParserBuilder OADefault String where
  buildParser _ = parseReadable str (Just "String")

instance ParserBuilder OADefault Text where
  buildParser _ = parseReadable auto (Just "Text")

instance ParserBuilder OADefault Day where
  buildParser _ =
    let dayReader = maybeReader (parseTimeM True defaultTimeLocale "%F")
    in parseReadable dayReader (Just "Day")

instance ParserBuilder OADefault TimeOfDay where
  buildParser _ =
    let dayReader = maybeReader (parseTimeM True defaultTimeLocale "%T")
    in parseReadable dayReader (Just "TimeOfDay")

instance ParserBuilder OADefault a => ParserBuilder OADefault (Maybe a) where
  buildParser _ mf mmA = optional $ maybe (buildParser OADefault mf Nothing) (buildParser OADefault mf) mmA

parseArgumentsToList :: (Show a, Read a) => ReadM a -> Text -> Text -> Parser [a]
parseArgumentsToList reader mv helpText = some $ argument reader (help (unpack helpText) <> metavar (unpack mv))

-- utilities to simplify deriving instances for specific types
enumAsExclusiveFlags :: (Enum e, Bounded e, Show e) => Maybe FieldName -> Maybe e -> Parser e
enumAsExclusiveFlags mf mE = foldl (<|>) empty $ map (\ev -> fl ev (optDesc <> (shortAndLong (toLower <$> show ev)))) [minBound..] where
  fl = maybe flag' flag mE
  optDesc = maybe mempty help mf




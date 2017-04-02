{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE UndecidableInstances  #-}
module DataBuilder.OptionParser
       (
         makeOAParser
       , ParserBuilder(..)
       , Identity
       ) where

import           Control.Monad         (join)
import           Data.Char             (toLower)
import           Data.Functor.Identity (Identity (..))
import           Data.Maybe            (fromJust)
import           Data.Monoid           ((<>))
import           Data.Validation       (AccValidation (..))
import           DataBuilder.TH        (deriveBuilder, handleJustL,
                                         handleNothingL)
import           DataBuilder.Types
import           Language.Haskell.TH
import           Options.Applicative

--instance Validatable Maybe a
--instance MonadLike Maybe

--type DBOAParser = FGV Parser Identity Maybe

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
  simpleBFail msg = abortOption (ErrorMsg $ msg) mempty <*> option disabled mempty
  simpleBSum = sumToCommand
--  bCollapse = runIdentity
--  bDistributeList = Identity . fmap runIdentity
  

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
    Nothing->argument reader ((maybe mempty Options.Applicative.value ma) <> (maybe mempty help mHelp))
    Just fieldName -> option reader ((maybe mempty Options.Applicative.value ma) <> (shortAndLong fieldName) <> (maybe mempty help mHelp))

instance ParserBuilder Int where
  buildParser = parseReadable auto (Just "Int") 

instance ParserBuilder Double where
  buildParser = parseReadable auto (Just "Double")

instance ParserBuilder String where
  buildParser = parseReadable str (Just "String")

instance {-# OVERLAPPABLE #-} (Show e,Enum e,Bounded e)=>ParserBuilder e where
  buildParser mf mE = foldl (<|>) empty $ map (\ev->fl ev (optDesc <> (shortAndLong (toLower <$> show ev)))) [minBound :: e..] where
    fl = maybe flag' flag mE
    optDesc = maybe mempty help mf

instance {-# OVERLAPPABLE #-} ParserBuilder a=>ParserBuilder (Maybe a) where
  buildParser mf mmA = optional $ maybe (buildParser mf Nothing) (buildParser mf) mmA


{-
instance Builder Parser Identity Identity Int where
  buildValidated _ mf = parseReadable auto (Just "Int") mf . fmap runIdentity 

instance Builder Parser Identity Double where
  buildValidated _ = parseReadable auto (Just "Double")

instance Builder Parser Identity String where
  buildValidated _ = parseReadable str (Just "String")

instance {-# OVERLAPPABLE #-} (Show e,Enum e,Bounded e)=>Builder Parser Identity e where
  buildValidated  _ mf mE = fToFV $ foldl (<|>) empty $ map (\ev->fl ev (optDesc <> (shortAndLong (toLower <$> show ev)))) [minBound :: e..] where
    fl = maybe flag' flag mE
    optDesc = maybe mempty help mf

instance {-# OVERLAPPABLE #-} Builder Parser Identity a=>Builder Parser Identity (Maybe a) where
  buildValidated _ mf mmA = optional $ maybe (buildA mf Nothing) (buildA mf) mmA


deriveOABuilder::Name -> Q [Dec]
deriveOABuilder typeName = do
  [d|instance Builder Parser Identity $(conT typeName) where
       buildValidated va mf Nothing  = $(handleNothingL typeName) va mf
       buildValidated va mf (Just x) = $(handleJustL typeName) va mf x|]

-}


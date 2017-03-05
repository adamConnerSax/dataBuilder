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
--       , deriveOABuilder
       , Identity
       ) where

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

instance Validatable Identity a

type DBOAParser = FGV Parser Identity Identity

type MDWrappedOA a = MDWrapped Parser Identity Identity a

collapse::DBOAParser a->Parser a
collapse = fmap runIdentity . fmap runIdentity . unFGV

type OABuilderC a = Builder Parser Identity Identity a

class ParserBuilder a where
  buildParser::Maybe FieldName->Maybe a->DBOAParser a
  default buildParser::(GBuilder Parser Identity Identity a, Validatable Identity a)=>Maybe FieldName->Maybe a->DBOAParser a
  buildParser mFN = gBuildValidated validator mFN . fmap Identity

instance ParserBuilder a=>Builder Parser Identity Identity a where
  buildValidated _ mf = buildParser mf . fmap runIdentity 
  
makeOAParser::OABuilderC a=>Maybe a->Parser a
makeOAParser = collapse . buildA Nothing . fmap pure

instance Buildable Parser Identity Identity where
  bFail msg = fToFGV $ abortOption (ErrorMsg $ msg) mempty <*> option disabled mempty
  bSum = sumToCommand
  bCollapse = runIdentity
  bDistributeList = Identity . fmap runIdentity
  

-- derive a command parser from a sum-type
sumToCommand::[MDWrappedOA a]->DBOAParser a
sumToCommand mdws =
  let makeCommand mdw = command (fst . metadata $ mdw) (info (collapse $ DataBuilder.Types.value mdw) mempty)
  in fToFGV $ subparser $ mconcat (map makeCommand mdws)

--shortAndLong::HasName f=>String->Mod f a
shortAndLong x = long x <> short (head x)

parseReadable::(Read a,Show a)=>ReadM a->Maybe String->Maybe FieldName->Maybe a->DBOAParser a
parseReadable reader mHelp mf ma =
  fToFGV $ case mf of
    Nothing->argument reader ((maybe mempty Options.Applicative.value ma) <> (maybe mempty help mHelp))
    Just fieldName -> option reader ((maybe mempty Options.Applicative.value ma) <> (shortAndLong fieldName) <> (maybe mempty help mHelp))

instance ParserBuilder Int where
  buildParser = parseReadable auto (Just "Int") 

instance ParserBuilder Double where
  buildParser = parseReadable auto (Just "Double")

instance ParserBuilder String where
  buildParser = parseReadable str (Just "String")

instance {-# OVERLAPPABLE #-} (Show e,Enum e,Bounded e)=>ParserBuilder e where
  buildParser mf mE = fToFGV $ foldl (<|>) empty $ map (\ev->fl ev (optDesc <> (shortAndLong (toLower <$> show ev)))) [minBound :: e..] where
    fl = maybe flag' flag mE
    optDesc = maybe mempty help mf

instance {-# OVERLAPPABLE #-} ParserBuilder a=>ParserBuilder (Maybe a) where
  buildParser mf mmA = fToFGV . optional . collapse $ maybe (buildParser mf Nothing) (buildParser mf) mmA


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


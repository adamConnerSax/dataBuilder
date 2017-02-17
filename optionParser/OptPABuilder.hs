{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
module OptPABuilder
       (
         makeOAParser
--       , deriveOABuilder
       ) where

import           Data.Char             (toLower)
import           Data.Functor.Identity (Identity (..))
import           Data.Maybe            (fromJust)
import           Data.Monoid           ((<>))
import           Data.Validation       (AccValidation (..))
--import           DataBuilder.TH        (deriveBuilder, handleJustL,
--                                        handleNothingL)
import           DataBuilder.Types
import           Language.Haskell.TH
import           Options.Applicative

instance MonadLike Identity -- uses defaults since Identity is a monad
instance Validatable Identity a
--type OptABuilder = Builder Parser Identity

collapse::FV Parser Identity a->Parser a
collapse = fmap runIdentity . unFV

makeOAParser::Builder Parser Identity a=>Maybe a->Parser a
makeOAParser = collapse . buildA Nothing

instance Buildable Parser Identity where
  bFail msg = fToFV $ abortOption (ErrorMsg $ msg) mempty <*> option disabled mempty
  bSum = sumToCommand

-- derive a command parser from a sum-type
sumToCommand::[MDWrapped Parser Identity a]->FV Parser Identity a
sumToCommand mdws =
  let makeCommand mdw = command (fst . metadata $ mdw) (info (collapse $ DataBuilder.Types.value mdw) mempty)
  in fToFV $ subparser $ mconcat (map makeCommand mdws)

--shortAndLong::HasName f=>String->Mod f a
shortAndLong x = long x <> short (head x)

parseReadable::(Read a,Show a)=>ReadM a->Maybe String->Maybe FieldName->Maybe a->FV Parser Identity a
parseReadable reader mHelp mf ma =
  fToFV $ case mf of
    Nothing->argument reader ((maybe mempty Options.Applicative.value ma) <> (maybe mempty help mHelp))
    Just fieldName -> option reader ((maybe mempty Options.Applicative.value ma) <> (shortAndLong fieldName) <> (maybe mempty help mHelp))

instance Builder Parser Identity Int where
  buildValidated _ = parseReadable auto (Just "Int")

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

{-
deriveOABuilder::Name -> Q [Dec]
deriveOABuilder typeName = do
  [d|instance Builder Parser $(conT typeName) where
       buildValidated va mf Nothing  = $(handleNothingL typeName) va mf
       buildValidated va mf (Just x) = $(handleJustL typeName) va mf x|]

-}


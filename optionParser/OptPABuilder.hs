{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
module OptPABuilder
       (
         makeOAParser
       , deriveOABuilder
       ) where

import           Data.Char           (toLower)
import           Data.Maybe          (fromJust)
import           Data.Monoid         ((<>))
import           Data.Monoid         ((<>))
import           Data.Text           (Text, unpack)
import           Data.Validation     (AccValidation (..))
import           DataBuilder.TH      (deriveBuilder, handleJustL,
                                      handleNothingL)
import           DataBuilder.Types
import           Language.Haskell.TH
import           Options.Applicative

makeOAParser::Builder Parser Text a=>Maybe a->Parser (AccValidation Text a)
makeOAParser = unVF . buildA Nothing

instance Buildable Parser Text where
  noConsError = "No constructors in sum type!"
  bFail msg = VF . fmap AccSuccess $ abortOption (ErrorMsg $ unpack msg) mempty <*> option disabled mempty
  bSum = sumToCommand

-- derive a command parser from a sum-type
sumToCommand::[MDWrapped Parser Text a]->VF Parser Text a
sumToCommand mdws =
  let makeCommand mdw = command (fst . metadata $ mdw) (info (unVF $ DataBuilder.Types.value mdw) mempty)
  in VF $ subparser $ mconcat (map makeCommand mdws)

--shortAndLong::HasName f=>String->Mod f a
shortAndLong x = long x <> short (head x)

parseReadable::(Read a,Show a)=>ReadM a->Maybe String->Maybe FieldName->Maybe a->VF Parser Text a
parseReadable reader mHelp mf ma =
  VF . fmap AccSuccess $ case mf of
    Nothing->argument reader ((maybe mempty Options.Applicative.value ma) <> (maybe mempty help mHelp))
    Just fieldName -> option reader ((maybe mempty Options.Applicative.value ma) <> (shortAndLong fieldName) <> (maybe mempty help mHelp))

instance Builder Parser Text Int where
  buildA = parseReadable auto (Just "Int")

instance Builder Parser Text Double where
  buildA = parseReadable auto (Just "Double")

instance Builder Parser Text String where
  buildA = parseReadable str (Just "String")

instance {-# OVERLAPPABLE #-} (Show e,Enum e,Bounded e)=>Builder Parser Text e where
  buildA mf mE = VF . fmap AccSuccess $ foldl (<|>) empty $ map (\ev->fl ev (optDesc <> (shortAndLong (toLower <$> show ev)))) [minBound :: e..] where
    fl = maybe flag' flag mE
    optDesc = maybe mempty help mf

instance {-# OVERLAPPABLE #-} Builder Parser Text a=>Builder Parser Text (Maybe a) where
  buildA mf mmA = optional $ maybe (buildA mf Nothing) (buildA mf) mmA

deriveOABuilder::Name -> Q [Dec]
deriveOABuilder typeName = do
  [d|instance Builder Parser $(conT typeName) where
       buildA mf Nothing  = $(handleNothingL typeName) mf
       buildA mf (Just x) = $(handleJustL typeName) mf x|]




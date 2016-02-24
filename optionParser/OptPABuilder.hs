{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module OptPABuilder
       (
         OPBOptions(..)
       , OPBMDH
       , typeOnlyOPBMDH
       , buildM
       , deriveBuilder
       ) where

import DataBuilder.Types
import Data.Maybe (fromJust)
import Data.Char (toLower)
import Options.Applicative
import DataBuilder.TH (deriveBuilder)

data OPBOptions = OPBOptions { showDefaults::Bool }
data OPBMDH = OPBMDH { options::OPBOptions, metadata::Metadata }

typeOnlyOPBMDH::OPBOptions->TypeName->OPBMDH
typeOnlyOPBMDH op tn = OPBMDH op (typeOnlyMD tn)

sdOption (OPBMDH op _) = if (showDefaults op) then showDefault else mempty

instance HasMetadata OPBMDH where
  getMetadata = metadata
  setMetadata md' (OPBMDH op _) = OPBMDH op md' 

instance Buildable Parser OPBMDH where
  bInject = pure 
  bApply = (<*>)
  bFail msg = abortOption (ErrorMsg msg) mempty <*> option disabled mempty
  bSum = sumToCommand

-- derive a command parser from a sum-type
sumToCommand::[MDWrapped Parser OPBMDH a]->Parser a
sumToCommand mdws =
  let makeCommand mdw = command (fromJust $ mdwConName mdw) (info (DataBuilder.Types.value mdw) mempty)
  in subparser $ mconcat (map makeCommand mdws)

--shortAndLong::HasName f=>String->Mod f a
shortAndLong x = long x <> short (head x)

parseReadable::(Read a,Show a)=>ReadM a->OPBMDH->Maybe a->Parser a
parseReadable reader opbmdh ma = case (fieldName . getMetadata $ opbmdh) of
  Nothing->argument reader (maybe mempty Options.Applicative.value ma)
  Just fieldName -> option reader ((maybe mempty Options.Applicative.value ma) <> (shortAndLong fieldName) <> sdOption opbmdh)

instance Builder Parser OPBMDH Int where
  buildM = parseReadable auto

instance Builder Parser OPBMDH Double where
  buildM = parseReadable auto

instance Builder Parser OPBMDH String where
  buildM = parseReadable str

instance {-# OVERLAPPABLE #-} (Show e,Enum e,Bounded e)=>Builder Parser OPBMDH e where
  buildM opbmdh mE = foldl (<|>) empty $ map (\ev->fl ev (optDesc <> (shortAndLong (toLower <$> show ev)) <> sdOption opbmdh)) [minBound :: e..] where
    fl = maybe flag' flag mE
    optDesc = maybe mempty help (fieldName . getMetadata $ opbmdh) 

instance {-# OVERLAPPABLE #-} Builder Parser OPBMDH a=>Builder Parser OPBMDH (Maybe a) where
  buildM opbmdh mmA = optional $ maybe (buildM opbmdh Nothing) (buildM opbmdh) mmA 




{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-} --just for demo.  Allows polymorphic result printer
module OptPABuilder where

import DataBuilderTH
import Data.Maybe (fromJust)
import Data.Char (toLower)
import Options.Applicative


instance Buildable Parser Metadata where
  bInject = pure 
  bApply = (<*>)
  bFail msg = abortOption (ErrorMsg msg) mempty <*> option disabled mempty
  bSum = sumToCommand

-- derive a command parser from a sum-type
sumToCommand::[MDWrapped Parser Metadata a]->Parser a
sumToCommand mdws =
  let makeCommand mdw = command (fromJust $ mdwConName mdw) (info (DataBuilderTH.value mdw) mempty)
  in subparser $ mconcat (map makeCommand mdws)

--shortAndLong::HasName f=>String->Mod f a
shortAndLong x = long x <> short (head x)

parseReadable::Read a=>ReadM a->Metadata->Maybe a->Parser a
parseReadable reader md ma = case (fieldName md) of
  Nothing->argument reader (maybe mempty Options.Applicative.value ma)
  Just fieldName -> option reader ((maybe mempty Options.Applicative.value ma) <> (shortAndLong fieldName))

instance Builder Parser Metadata Int where
  buildM = parseReadable auto

instance Builder Parser Metadata Double where
  buildM = parseReadable auto

instance Builder Parser Metadata String where
  buildM = parseReadable str

instance {-# OVERLAPPABLE #-} (Show e,Enum e,Bounded e)=>Builder Parser Metadata e where
  buildM md mE = foldl (<|>) empty $ map (\ev->fl ev (optDesc <> (shortAndLong (toLower <$> show ev)))) [minBound :: e..] where
    fl = maybe flag' flag mE
    optDesc = maybe mempty help (fieldName md) 

instance {-# OVERLAPPABLE #-} Builder Parser Metadata a=>Builder Parser Metadata (Maybe a) where
  buildM md mmA = optional $ maybe (buildM md Nothing) (buildM md) mmA 





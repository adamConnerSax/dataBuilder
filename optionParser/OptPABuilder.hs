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

instance Buildable Parser where
  bInject = pure 
  bApply = (<*>)
  bFail msg = abortOption (ErrorMsg msg) mempty <*> option disabled mempty
  bSum = sumToCommand

-- derive a command parser from a sum-type
sumToCommand::[MDWrapped Parser a]->Parser a
sumToCommand mdws = case (buildersHaveConNames mdws) of
  False -> bFail "Sum type for command encountered but constructor name(s) are missing."
  True ->
    let makeCommand mdw = command (fromJust $ mdwCN mdw) (info (DataBuilderTH.value mdw) mempty)
    in subparser $ mconcat (map makeCommand mdws)

mdwCN::MDWrapped f a -> Maybe ConName
mdwCN x = conName (metadata x) 

mdwHasConName::MDWrapped f a->Bool
mdwHasConName mdw = maybe False (const True) (mdwCN mdw) -- Lens??

buildersHaveConNames::[MDWrapped f a]->Bool
buildersHaveConNames bes = null (filter (not . mdwHasConName) bes) 
--
--shortAndLong::HasName f=>String->Mod f a
shortAndLong x = long x <> short (head x)

parseReadable::Read a=>ReadM a->Metadata->Maybe a->Parser a
parseReadable reader md ma = case (fieldName md) of
  Nothing->argument reader (maybe mempty Options.Applicative.value ma)
  Just fieldName -> option reader ((maybe mempty Options.Applicative.value ma) <> (shortAndLong fieldName))

instance Builder Parser Int where
  buildM = parseReadable auto

instance Builder Parser Double where
  buildM = parseReadable auto

instance Builder Parser String where
  buildM = parseReadable str

instance {-# OVERLAPPABLE #-} (Show e,Enum e,Bounded e)=>Builder Parser e where
  buildM md mE = foldl (<|>) empty $ map (\ev->fl ev (optDesc <> (shortAndLong (toLower <$> show ev)))) [minBound :: e..] where
    fl = maybe flag' flag mE
    optDesc = maybe mempty help (fieldName md) 


instance {-# OVERLAPPABLE #-} Builder Parser a=>Builder Parser (Maybe a) where
  buildM md mmA = optional $ maybe (buildM md Nothing) (buildM md) mmA 





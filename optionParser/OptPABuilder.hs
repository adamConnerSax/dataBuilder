{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module OptPABuilder
       (
         makeOAParser
       , deriveOABuilder
       ) where

import DataBuilder.Types
import Data.Maybe (fromJust)
import Data.Char (toLower)
import Options.Applicative
import DataBuilder.TH (deriveBuilder,handleNothingL,handleJustL)
import Language.Haskell.TH


makeOAParser::Builder Parser a=>Maybe a->Parser a
makeOAParser = buildM (typeOnlyMD "")

instance Buildable Parser where
  bMap = fmap 
  bInject = pure 
  bApply = (<*>)
  bFail msg = abortOption (ErrorMsg msg) mempty <*> option disabled mempty
  bSum = sumToCommand

-- derive a command parser from a sum-type
sumToCommand::[MDWrapped Parser a]->Parser a
sumToCommand mdws =
  let makeCommand mdw = command (fromJust . conName . metadata $ mdw) (info (DataBuilder.Types.value mdw) mempty)
  in subparser $ mconcat (map makeCommand mdws)

--shortAndLong::HasName f=>String->Mod f a
shortAndLong x = long x <> short (head x)

parseReadable::(Read a,Show a)=>ReadM a->Maybe String->Metadata->Maybe a->Parser a
parseReadable reader mHelp md ma =
  case (fieldName md) of
    Nothing->argument reader ((maybe mempty Options.Applicative.value ma) <> (maybe mempty help mHelp))
    Just fieldName -> option reader ((maybe mempty Options.Applicative.value ma) <> (shortAndLong fieldName) <> (maybe mempty help mHelp))

instance Builder Parser Int where
  buildM = parseReadable auto (Just "Int")

instance Builder Parser Double where
  buildM = parseReadable auto (Just "Double")

instance Builder Parser String where
  buildM = parseReadable str (Just "String")

instance {-# OVERLAPPABLE #-} (Show e,Enum e,Bounded e)=>Builder Parser e where
  buildM md mE = foldl (<|>) empty $ map (\ev->fl ev (optDesc <> (shortAndLong (toLower <$> show ev)))) [minBound :: e..] where
    fl = maybe flag' flag mE
    optDesc = maybe mempty help (fieldName md) 

instance {-# OVERLAPPABLE #-} Builder Parser a=>Builder Parser (Maybe a) where
  buildM md mmA = optional $ maybe (buildM md Nothing) (buildM md) mmA 

deriveOABuilder::Name -> Q [Dec]
deriveOABuilder typeName = do
  [d|instance Builder Parser $(conT typeName) where
       buildM md Nothing  = $(handleNothingL typeName) md
       buildM md (Just x) = $(handleJustL typeName) md x|]




{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import DataBuilderTH
import Language.Haskell.TH
import OptPABuilder
import Options.Applicative
 

data Mode = Verbose | Silent deriving (Show,Enum,Bounded)
data Process = AProcess | BProcess deriving (Show,Enum,Bounded)
data Config = Config {input :: String, output :: Maybe String, mode  :: Maybe Mode, process :: Process } deriving (Show)

{- Also supports commands automatically if you derive the builder for a sum type
--data Commands = DoA {aFile::String} | DoB {bFile::String, bFlag::Bool} deriving (Show)
--data Config = Config {input :: String, output :: Maybe String, mode  :: Maybe Mode, process :: Process, cmd :: Commands  } deriving (Show)
--deriveBuilder ''Parser ''Commands
-}

deriveBuilder ''Parser ''Config

main::IO ()
main = (execParser $ info parser infoMod) >>= print where
--  parser = buildM (typeOnlyMD "Config") (Nothing :: Maybe Config) -- no defaults
  parser = buildM (typeOnlyMD "Config") (Just $ configDefault) -- supplies defaults for everything
  configDefault = Config "Hello" Nothing (Just Verbose) AProcess
  infoMod = fullDesc <> progDesc "Sample use of DataBauilder to create a parser from a data type"

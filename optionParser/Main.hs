{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-} --just for demo.  Allows polymorphic result printer
module Main where

import DataBuilderTH
import Language.Haskell.TH
import OptPABuilder
import Options.Applicative
 

data Mode = Verbose | Silent deriving (Show,Enum,Bounded)
data Process = AProcess | BProcess deriving (Show,Enum,Bounded)
data Commands = DoA {aFile::String} | DoB {bFile::String, bFlag::Bool} deriving (Show)
data Config = Config {input :: String, output :: Maybe String, mode  :: Maybe Mode, process :: Process, cmd :: Commands  } deriving (Show)




deriveBuilder ''Parser ''Commands
deriveBuilder ''Parser ''Config

main::IO ()
main = (execParser $ info parser infoMod) >>= print where
  parser = buildM (typeOnlyMD "ConfigSimple") (Nothing :: Maybe Config)
  infoMod = fullDesc <> progDesc "Sample use of DataBauilder to create a parser from a data type"

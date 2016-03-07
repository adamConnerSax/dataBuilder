{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import OptPABuilder
import Options.Applicative
import DataBuilder.TH (deriveBuilder)
import qualified GHC.Generics as GHCG
import DataBuilder.Types (Builder)
import DataBuilder.GenericSOP 



data Mode = Verbose | Silent deriving (Show,Enum,Bounded,GHCG.Generic)
instance Generic Mode
instance HasDatatypeInfo Mode

data Process = AProcess | BProcess deriving (Show,Enum,Bounded,GHCG.Generic)
instance Generic Process
instance HasDatatypeInfo Process

--data Config = Config {input :: String, output :: Maybe String, mode  :: Maybe Mode, process :: Process } deriving (Show,GHCG.Generic)


-- Also supports commands automatically if you derive the builder for a sum type
data Commands = DoA {aFile::String} | DoB {bFile::String, bFlag::Bool} deriving (Show,GHCG.Generic)
instance Generic Commands
instance HasDatatypeInfo Commands
instance Builder Parser OPBMDH Commands -- uses the generic version via generics-sop.  Requires SOP.Generic, SOP.HasDatatypeInfo

data Config = Config {input :: String, output :: Maybe String, mode  :: Maybe Mode, process :: Process, cmd :: Commands  } deriving (Show,GHCG.Generic)
--deriveBuilder ''Parser ''OPBMDH ''Commands



instance Generic Config
instance HasDatatypeInfo Config
instance Builder Parser OPBMDH Config -- uses the generic version via generics-sop.  Requires SOP.Generic, SOP.HasDatatypeInfo 


--deriveBuilder ''Parser ''OPBMDH ''Config --uses TH to build the instance.  Then you do not need the Generic or HasDatatypeInfo instances


main::IO ()
main = (execParser $ info (helper <*> parser) infoMod) >>= print where
  opbOptions = OPBOptions True
--  parser = buildM (typeOnlyOPBMDH opbOptions "Config") (Just $ configDefault) -- supplies defaults for everything
  configDefault = Config "Hello" Nothing (Just Verbose) AProcess
  infoMod = fullDesc <> progDesc "Sample use of DataBuilder to create a parser from a data type"
  parser = buildM (typeOnlyOPBMDH opbOptions "Config") (Nothing :: Maybe Config) -- no defaults

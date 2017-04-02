{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Data.Functor.Identity  (Identity)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import           DataBuilder.GenericSOP
import           DataBuilder.TH         (deriveBuilder)
import           DataBuilder.Types      (Builder)
import qualified GHC.Generics           as GHCG
import           Options.Applicative
import           DataBuilder.OptionParser

data Mode = Verbose | Silent deriving (Show,Enum,Bounded,GHCG.Generic)
instance Generic Mode
instance HasDatatypeInfo Mode

data Process = AProcess | BProcess deriving (Show,Enum,Bounded,GHCG.Generic)
instance Generic Process
instance HasDatatypeInfo Process

data Config = Config {input :: String, output :: Maybe String, mode  :: Maybe Mode, process :: Process } deriving (Show,GHCG.Generic)

instance Generic Config
instance HasDatatypeInfo Config
instance ParserBuilder  Config -- uses the generic version via generics-sop.  Requires SOP.Generic, SOP.HasDatatypeInfo


-- Also supports commands automatically if you derive the builder for a sum type
data Commands = DoA {aFile::String} | DoB {bFile::String, bFlag::Bool} deriving (Show,GHCG.Generic)
instance Generic Commands
instance HasDatatypeInfo Commands
instance ParserBuilder Commands -- uses the generic version via generics-sop.  Requires SOP.Generic, SOP.HasDatatypeInfo

data ConfigCmd = ConfigCmd {inputC :: String, outputC :: Maybe String, modeC  :: Maybe Mode, processC :: Process, command :: Commands } deriving (Show,GHCG.Generic)

--data Config = Config {input :: String, output :: Maybe String, mode  :: Maybe Mode, process :: Process, cmd :: Commands  } deriving (Show,GHCG.Generic)
--deriveBuilder ''Parser ''Commands



instance Generic ConfigCmd
instance HasDatatypeInfo ConfigCmd
instance ParserBuilder  ConfigCmd -- uses the generic version via generics-sop.  Requires SOP.Generic, SOP.HasDatatypeInfo


--deriveOABuilder ''Config --uses TH to build the instance.  Then you do not need the Generic or HasDatatypeInfo instances


main::IO ()
main = (execParser $ info (helper <*> parser) infoMod) >>= print where
--  parser = makeOAParser (Just $ configDefault) -- supplies defaults for everything
  parser = makeOAParser (Nothing :: Maybe Config) -- no defaults
--  parser = makeOAParser (Nothing :: Maybe Int)
  configDefault = Config "Hello" Nothing (Just Verbose) AProcess
  configCmdDefault = ConfigCmd "Hello" Nothing (Just Verbose) AProcess (DoA "fileName.txt")
  infoMod = fullDesc <> progDesc "Sample use of DataBuilder to create a parser from a data type"


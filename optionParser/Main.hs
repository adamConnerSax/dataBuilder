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
import           OptPABuilder

data Mode = Verbose | Silent deriving (Show,Enum,Bounded,GHCG.Generic)
instance Generic Mode
instance HasDatatypeInfo Mode

data Process = AProcess | BProcess deriving (Show,Enum,Bounded,GHCG.Generic)
instance Generic Process
instance HasDatatypeInfo Process

data Config = Config {input :: String, output :: Maybe String, mode  :: Maybe Mode, commands :: Commands } deriving (Show,GHCG.Generic)


-- Also supports commands automatically if you derive the builder for a sum type
data Commands = DoA {aFile::String} | DoB {bFile::String, bFlag::Bool} deriving (Show,GHCG.Generic)
instance Generic Commands
instance HasDatatypeInfo Commands
instance Builder Parser Identity Commands -- uses the generic version via generics-sop.  Requires SOP.Generic, SOP.HasDatatypeInfo

--data Config = Config {input :: String, output :: Maybe String, mode  :: Maybe Mode, process :: Process, cmd :: Commands  } deriving (Show,GHCG.Generic)
--deriveBuilder ''Parser ''Commands


{-
instance Generic Config
instance HasDatatypeInfo Config
instance Builder Parser Identity Config -- uses the generic version via generics-sop.  Requires SOP.Generic, SOP.HasDatatypeInfo
-}

deriveOABuilder ''Config --uses TH to build the instance.  Then you do not need the Generic or HasDatatypeInfo instances


main::IO ()
main = (execParser $ info (helper <*> parser) infoMod) >>= print where
  --parser = makeOAParser (Just $ configDefault) -- supplies defaults for everything
--  configDefault = Config "Hello" Nothing (Just Verbose) AProcess
  infoMod = fullDesc <> progDesc "Sample use of DataBuilder to create a parser from a data type"
  parser = makeOAParser (Nothing :: Maybe Config) -- no defaults

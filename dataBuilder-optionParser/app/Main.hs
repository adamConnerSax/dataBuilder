{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import           Data.Monoid              ((<>))
import           Data.Text                (Text, pack)

import qualified GHC.Generics             as GHCG

import           Options.Applicative

import           DataBuilder.OptionParser

data Mode = Verbose | Silent deriving (Show, Enum, Bounded, GHCG.Generic)
instance Generic Mode
instance HasDatatypeInfo Mode
instance ParserBuilder OADefault Mode where
  buildParser _ = enumAsExclusiveFlags

data Process = AProcess | BProcess deriving (Show, Enum, Bounded, GHCG.Generic)
instance Generic Process
instance HasDatatypeInfo Process
instance ParserBuilder OADefault Process where
  buildParser _ = enumAsExclusiveFlags

newtype ListOfArgs = ListOfArgs [Text] deriving (Show, GHCG.Generic)

instance Generic ListOfArgs
instance HasDatatypeInfo ListOfArgs
instance ParserBuilder OADefault ListOfArgs where
  buildParser _ _ _ = ListOfArgs . fmap pack <$> parseArgumentsToList str "ARGS" "List of (Text) Arguments"

data Config = Config {input :: String, output :: Maybe String, mode  :: Maybe Mode, process :: Process, args :: ListOfArgs } deriving (Show, GHCG.Generic)

instance Generic Config
instance HasDatatypeInfo Config
instance ParserBuilder OADefault Config -- uses the generic version via generics-sop.  Requires SOP.Generic, SOP.HasDatatypeInfo

-- Also supports commands automatically if you derive the builder for a sum type
data Commands = DoA { aFile :: String} | DoB {bFile :: String, bFlag :: Bool} deriving (Show,GHCG.Generic)
instance Generic Commands
instance HasDatatypeInfo Commands
instance ParserBuilder OADefault Commands -- uses the generic version via generics-sop.  Requires SOP.Generic, SOP.HasDatatypeInfo

data ConfigCmd = ConfigCmd {inputC :: String, outputC :: Maybe String, modeC  :: Maybe Mode, processC :: Process, command :: Commands } deriving (Show,GHCG.Generic)

instance Generic ConfigCmd
instance HasDatatypeInfo ConfigCmd
instance ParserBuilder OADefault ConfigCmd -- uses the generic version via generics-sop.  Requires SOP.Generic, SOP.HasDatatypeInfo

main :: IO ()
main = (execParser $ info (helper <*> parser) infoMod) >>= print where
--  parser = makeDefaultOAParser (Just $ configDefault) -- supplies defaults for everything
  parser = makeDefaultOAParser (Nothing :: Maybe Config) -- no defaults
  configDefault = Config "Hello" Nothing (Just Verbose) AProcess
--  configCmdDefault = ConfigCmd "Hello" Nothing (Just Verbose) AProcess (DoA "fileName.txt")
  infoMod = fullDesc <> progDesc "Sample use of DataBuilder to create a parser from a data type"


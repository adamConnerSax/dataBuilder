{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}


module Main where

import DataBuilder.Types
import DataBuilder.TH
import Data.List (intercalate,find)
import Text.Read (readMaybe)
import System.IO (hFlush,stdout)
import Data.Maybe (fromJust)

data TestNull = Null deriving (Show)
data TestOne = One Int deriving (Show)
data TestTwo = Two Int String deriving (Show)
data TestRecord = TestR { intF::Int, stringF::String } deriving (Show)
data TestSum = A | B Int | C Char Int | D Char Int TestRecord deriving (Show)
data TestNested = Nested Int String TestSum deriving (Show)


newtype BuilderEx a = BuilderEx { unBldr::IO (Maybe a) } deriving (Functor)


instance Applicative BuilderEx where
  pure x = BuilderEx $ return $ Just x
  bf <*> ba = BuilderEx $ do
    mf <- unBldr bf
    ma <- unBldr ba
    return $ mf <*> ma

instance Buildable BuilderEx where
  bFail msg = BuilderEx $ putStrLn msg >> return Nothing 
  bSum = sumBEs


-- the only tricky part.  How to handle sum types?
sumBEs::[MDWrapped BuilderEx a]->BuilderEx a
sumBEs mdws = BuilderEx $ do
  let starDefault mdw = (fst $ metadata mdw) ++ if hasDefault mdw then "*" else ""
      conNames = map starDefault mdws
      names = intercalate "," conNames
      prompt = "Type has multiple constructors. Please choose (" ++ names ++ "): "
  putStr prompt
  hFlush stdout
  chosen <- getLine
  let mMDW = find (\mdw -> chosen == (fst . metadata $ mdw)) mdws
  case mMDW of
    Nothing -> unBldr $ bFail (chosen ++ " unrecognized constructor!")
    Just mdw -> unBldr $ value mdw


-- You need base case builders in order to build at least primitive types
-- In general you would also need to handle lists, maps or traversables in general
simpleBuilder::(Show a, Read a)=>Maybe FieldName->Maybe a->BuilderEx a 
simpleBuilder mf Nothing = BuilderEx $ do
  let prompt = (maybe "" id mf) ++ ": " 
  putStr prompt
  hFlush stdout
  (readMaybe <$> getLine)

simpleBuilder mf (Just a) = BuilderEx $ do
  let prompt = (maybe "" id mf) ++ " (was " ++ show a ++ "): "
  putStr prompt
  hFlush stdout
  (readMaybe <$> getLine)

-- This is overlappable so that we can use the TH to derive this for things.
-- Otherwise this covers all things since the constraints don't restrict matching.
instance {-# OVERLAPPABLE #-} (Show a, Read a)=>Builder BuilderEx a where
  buildA = simpleBuilder 


deriveBuilder ''BuilderEx ''TestNull
deriveBuilder ''BuilderEx ''TestOne
deriveBuilder ''BuilderEx ''TestTwo
deriveBuilder ''BuilderEx ''TestSum
deriveBuilder ''BuilderEx ''TestRecord
deriveBuilder ''BuilderEx ''TestNested

g::Show a=>Maybe a->IO ()
g = maybe (putStrLn "built Nothing") (putStrLn . show) 


main::IO ()
main = do

  putStrLn "Given:\ndata TestTwo=Two Int String\ndata TestRecord = TestR { intF::Int, stringF::String }\ndata TestSum = A | B Int | C Char Int | D Char Int Bool deriving (Show)\ndata TestNested = Nested Int String TestSum deriving (Show)"
  putStrLn "Build a TestTwo from Nothing"
  unBldr (buildA Nothing (Nothing :: Maybe TestTwo)) >>= g
  putStrLn "Build a TestTwo from a given value (=Two 11 \"Hola\")"
  unBldr (buildA Nothing (Just $ Two 11 "Hola")) >>= g
  putStrLn "Build a TestRecord from a given value (=TestR 11 \"Hola\")"
  unBldr (buildA Nothing (Just $ TestR 11 "Hola")) >>= g
  putStrLn "Build a TestSum from a given value (=C 'a' 12)"
  unBldr (buildA Nothing (Just $ C 'a' 12)) >>= g
  putStrLn "Build a TestNested from Nothing"
  unBldr (buildA Nothing (Nothing :: Maybe TestNested)) >>= g
  putStrLn "Build a TestNested from a value (=TestNested 12 \"Hello\" (D 'a' 2 (TestR 5 \"Adios\"))"
  unBldr (buildA Nothing (Just (Nested 12 "Hello" (D 'a' 2 (TestR 5 "Adios"))))) >>= g
 

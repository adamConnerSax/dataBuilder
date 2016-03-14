{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-} --just for demo.  Allows polymorphic result printer

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


newtype BuilderEx a = BuilderEx { bldr::IO (Maybe a) }

instance Functor BuilderEx where
  fmap f bea = BuilderEx $ (fmap f) <$> bldr bea

instance Buildable BuilderEx where
  bMap = fmap 
  bInject x = BuilderEx $ return (Just x)
  bApply bAB bA = BuilderEx $ do
    mAB <- (bldr bAB)
    mA <-  (bldr bA)
    return $ mAB <*> mA
  bFail msg = BuilderEx $ putStrLn msg >> return Nothing 
  bSum = sumBEs


-- the only tricky part.  How to handle sum types?
sumBEs::[MDWrapped BuilderEx a]->BuilderEx a
sumBEs mdws = BuilderEx $ do
  let starDefault mdw = fromJust (conName $ metadata mdw) ++ if hasDefault mdw then "*" else ""
      conNames = map starDefault mdws
      names = intercalate "," conNames
      prompt = "Type has multiple constructors. Please choose (" ++ names ++ "): "
  putStr prompt
  hFlush stdout
  chosen <- getLine
  let mMDW = find (\mdw -> chosen == (fromJust . conName . metadata $ mdw)) mdws
  case mMDW of
    Nothing -> bldr $ bFail (chosen ++ " unrecognized constructor!")
    Just mdw -> bldr $ value mdw


-- You need base case builders in order to build at least primitive types
-- In general you would also need to handle lists, maps or traversables in general
simpleBuilder::(Show a, Read a)=>Metadata->Maybe a->BuilderEx a 
simpleBuilder md Nothing = BuilderEx $ do
  let prompt = (maybe "" (\x->x++"::") (fieldName md)) ++ (typeName md) ++ ": " 
  putStr prompt
  hFlush stdout
  (readMaybe <$> getLine)

simpleBuilder md (Just a) = BuilderEx $ do
  let prompt = (maybe "" (\x->x++"::") (fieldName md)) ++ (typeName md) ++ " (was " ++ show a ++ "): "
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

g = maybe (putStrLn "built Nothing") (putStrLn . show) 


main::IO ()
main = do

  putStrLn "Given:\ndata TestTwo=Two Int String\ndata TestRecord = TestR { intF::Int, stringF::String }\ndata TestSum = A | B Int | C Char Int | D Char Int Bool deriving (Show)\ndata TestNested = Nested Int String TestSum deriving (Show)"
  putStrLn "Build a TestTwo from Nothing"
  bldr (buildA (typeOnlyMD "TestTwo") (Nothing :: Maybe TestTwo)) >>= g
  putStrLn "Build a TestTwo from a given value (=Two 11 \"Hola\")"
  bldr (buildA (typeOnlyMD "TestTwo") (Just $ Two 11 "Hola")) >>= g
  putStrLn "Build a TestRecord from a given value (=TestR 11 \"Hola\")"
  bldr (buildA (typeOnlyMD "TestRecord") (Just $ TestR 11 "Hola")) >>= g
  putStrLn "Build a TestSum from a given value (=C 'a' 12)"
  bldr (buildA (typeOnlyMD "TestSum") (Just $ C 'a' 12)) >>= g
  putStrLn "Build a TestNested from Nothing"
  bldr (buildA (typeOnlyMD "TestNested") (Nothing :: Maybe TestNested)) >>= g
  putStrLn "Build a TestNested from a value (=TestNested 12 \"Hello\" (D 'a' 2 (TestR 5 \"Adios\"))"
  bldr (buildA (typeOnlyMD "TestNested") (Just (Nested 12 "Hello" (D 'a' 2 (TestR 5 "Adios"))))) >>= g
 

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-} --just for demo.  Allows polymorphic result printer
module Main where

import DataBuilderTH
import Language.Haskell.TH
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

instance Buildable BuilderEx where
  bInject x = BuilderEx $ return (Just x)
  bApply bAB bA = BuilderEx $ do
    mAB <- (bldr bAB)
    mA <-  (bldr bA)
    return $ mAB <*> mA
  bSum = sumBEs


-- the only tricky part.  How to handle sum types?
sumBEs::[MDWrapped BuilderEx a]->BuilderEx a
sumBEs mws = case (length mws) of
  0 -> BuilderEx $ putStrLn "No constructors in sumBEs" >> return Nothing
  1 -> value (head mws)
  _ -> BuilderEx $ 
    case (buildersHaveConNames mws) of
      False -> putStrLn "At least one constructor is missing metadata in sumBEs!" >> return Nothing
      True -> do
        let starDefault mdw = fromJust (mdwCN mdw) ++ if hasDefault mdw then "*" else ""
            conNames = map starDefault mws
            names = intercalate "," conNames
            prompt = "Type has multiple constructors. Please choose (" ++ names ++ "): "
        putStr prompt
        hFlush stdout
        chosen <- getLine
        let mMDW = find (\mdw -> chosen == (fromJust (mdwCN mdw))) mws
        case mMDW of
          Nothing -> putStrLn (chosen ++ " unrecognized constructor!") >> return Nothing
          Just mdw -> bldr $ value mdw


mdwCN::MDWrapped f a -> Maybe ConName
mdwCN x = conName (metadata x) 

mdwHasConName::MDWrapped f a->Bool
mdwHasConName mdw = maybe False (const True) (mdwCN mdw) -- Lens??

buildersHaveConNames::[MDWrapped f a]->Bool
buildersHaveConNames bes = null (filter (not . mdwHasConName) bes) 
--


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
  buildM = simpleBuilder 


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
  bldr (buildM (typeOnlyMD "TestTwo") (Nothing :: Maybe TestTwo)) >>= g
  putStrLn "Build a TestTwo from a given value (=Two 11 \"Hola\")"
  bldr (buildM (typeOnlyMD "TestTwo") (Just $ Two 11 "Hola")) >>= g
  putStrLn "Build a TestRecord from a given value (=TestR 11 \"Hola\")"
  bldr (buildM (typeOnlyMD "TestRecord") (Just $ TestR 11 "Hola")) >>= g
  putStrLn "Build a TestSum from a given value (=C 'a' 12)"
  bldr (buildM (typeOnlyMD "TestSum") (Just $ C 'a' 12)) >>= g
  putStrLn "Build a TestNested from Nothing"
  bldr (buildM (typeOnlyMD "TestNested") (Nothing :: Maybe TestNested)) >>= g
  putStrLn "Build a TestNested from a value (=TestNested 12 \"Hello\" (D 'a' 2 (TestR 5 \"Adios\"))"
  bldr (buildM (typeOnlyMD "TestNested") (Just (Nested 12 "Hello" (D 'a' 2 (TestR 5 "Adios"))))) >>= g
 

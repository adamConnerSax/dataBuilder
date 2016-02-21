{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-} --just for demo.  Allows polymorphic result printer
module Main where

import DataBuilderTH
import Control.Monad (ap)
import Language.Haskell.TH
--import Language.Haskell.TH.Lift
import Data.Functor.Identity
import Data.List (intercalate,find)
import Text.Read (readMaybe)
import System.IO (hFlush,stdout)
import Data.Data (Data)
import Data.Maybe (fromJust)

data TestNull = Null deriving (Show,Data)
data TestOne = One Int deriving (Show,Data)
data TestTwo = Two Int String deriving (Show,Data)
data TestSum = A | B Int | C Char Int | D Char Int Bool deriving (Show,Data)
data TestRecord = TestR { intF::Int, stringF::String } deriving (Show,Data)
data TestNested = Nested Int String TestSum deriving (Show,Data)


newtype BuilderEx a = BuilderEx { bldr::IO (Maybe a) }

instance Buildable  BuilderEx where
  bInject x = BuilderEx $ return (Just x)
  bApply bAB bA = BuilderEx $ do
    mAB <- (bldr bAB)
    mA <-  (bldr bA)
    return $ mAB <*> mA
  bSum = sumBEs

--metadataHasConName::Maybe Metadata->Bool
--metadataHasConName mMD = maybe False (\md->maybe False (const True) (conName md)) mMD

mdwCN::MDWrapped f a -> Maybe ConName
mdwCN x = conName (metadata x) 

mdwHasConName::MDWrapped f a->Bool
mdwHasConName mdw = maybe False (const True) (mdwCN mdw) -- Lens??

buildersHaveConNames::[MDWrapped f a]->Bool
buildersHaveConNames bes = null (filter (not . mdwHasConName) bes) 

{-
buildChooseTuple::BuilderEx a->IO (String,ConName,MW a)
buildChooseTuple bA = do
  mw@(MW mMD mA) <- bldr bA
  let hasValue = maybe False (const True) mA
      conName = (\(Metadata _ mC _)->fromJust mC) (fromJust mMD)
      chooserS = conName ++ if hasValue then "*" else ""
  return (chooserS,conName,mw)
-}


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


-- You need base case builders.  How to build basic types
simpleBuilder::(Show a, Read a)=>Metadata->Maybe a->BuilderEx a 
simpleBuilder md Nothing = BuilderEx $ (putStr "Enter: " >> hFlush stdout >> (readMaybe <$> getLine))
simpleBuilder md (Just a) = BuilderEx $ (putStr ("Enter (was=" ++ show a ++ "):") >> hFlush stdout >> (readMaybe <$> getLine))

-- This is overlappable so that we can use the TH to derive this for things.  Otherwise this covers all things!
instance {-# OVERLAPPABLE #-} (Show a, Read a)=>Builder BuilderEx a where
  buildM = simpleBuilder 


g = maybe (putStrLn "built Nothing") (putStrLn . show) 

deriveBuild ''BuilderEx ''TestNull
deriveBuild ''BuilderEx ''TestOne
deriveBuild ''BuilderEx ''TestTwo
deriveBuild ''BuilderEx ''TestSum
deriveBuild ''BuilderEx ''TestRecord
deriveBuild ''BuilderEx ''TestNested


main::IO ()
main = do

  putStrLn "Given:\ndata TestTwo=Two Int String\ndata TestSum = A | B Int | C Char Int | D Char Int Bool deriving (Show)\ndata TestNested = Nested Int String TestSum deriving (Show)"
  putStrLn "Build a TestTwo from Nothing"
  bldr (buildM (typeOnlyMD "TestTwo") (Nothing :: Maybe TestTwo)) >>= g
  putStrLn "Build a TestTwo from a given value (=Two 11 \"Hola\")"
  bldr (buildM (typeOnlyMD "TestTwo") (Just $ Two 11 "Hola")) >>= g
  putStrLn "Build a TestSum from a given value (=C 'a' 12)"
  bldr (buildM (typeOnlyMD "TestSum") (Just $ C 'a' 12)) >>= g
  putStrLn "Build a TestNested from Nothing"
  bldr (buildM (typeOnlyMD "TestNested") (Nothing :: Maybe TestNested)) >>= g
  putStrLn "Build a TestSum from a value (=TestNested 12 \"Hello\" (D 'a' 2 True)"
  bldr (buildM (typeOnlyMD "TestNested") (Just $ Nested 12 "Hello" (D 'a' 2 True))) >>= g
  


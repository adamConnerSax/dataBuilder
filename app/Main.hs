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

data TestNull = Null deriving (Show,Data)
data TestOne = One Int deriving (Show,Data)
data TestTwo = Two Int String deriving (Show,Data)
data TestSum = A | B Int | C Char Int | D Char Int Bool deriving (Show,Data)
data TestRecord = TestR { intF::Int, stringF::String } deriving (Show,Data)
data TestNested = Nested Int String TestSum deriving (Show,Data)

deriveBuild ''TestNull
deriveBuild ''TestOne
deriveBuild ''TestTwo
deriveBuild ''TestSum
deriveBuild ''TestRecord
deriveBuild ''TestNested

data BuilderEx a = BuilderEx (Maybe Metadata) (IO (Maybe a))

builtIO::BuilderEx a -> IO (Maybe a)
builtIO (BuilderEx _ ioma) = ioma

sumBEs::[BuilderEx a]->BuilderEx a
sumBEs bes = case (length bes) of
  0 -> putStrLn "No constructors in sumBEs" >> return Nothing
  1 -> head bes
  _ -> do
    let conName (BuilderEx (Metadata _ con _)) = con
        toName be = conName be ++ if not bIsNothing be then "*" else ""
        names = intercalate "," $ map toName bes
        prompt = "Type has multiple constructors. Please choose (" ++ names ++ "): "
    putStr prompt
    hFlush stdout
    cid <- getLine
    let mBe = find (\be->conName be == cid) bes
    case mBe of
      Nothing -> putStrLn (cid ++ " unrecognized constructor!") >> return Nothing
      Just be -> be

instance Buildable BuilderEx where
  bInject md x = BuilderEx md (return (Just x))
  bApply (BuilderEx mAB iomAB) (BuilderEx mA iomA) = BuilderEx mAB iomB where
    iomB = do
      mf <- iomAB
      ma <- iomA
      return $ mf <*> ma
  bNothing = BuilderEx Nothing Nothing
  bSum = sumBEs
  bMeta (BuilderEx md _) = md
  bIsNothing (BuilderEx _ ma) = (ma == return Nothing) 
   

-- You need base case builders.  How to build basic types
simpleBuilder::(Show a, Read a, Data a)=>Maybe FieldName->Maybe a->BuilderEx a 
simpleBuilder mFN Nothing = BuilderEx (gatherMetaData undefined::a) (putStr "Enter: " >> hFlush stdout >> ((readMaybe <$> getLine)))
simpleBuilder mFN (Just a) = BuilderEx (gatherMetaData undefined::a) (putStr ("Enter (was=" ++ show a ++ "):") >> hFlush stdout >> ((readMaybe <$> getLine)))

-- This is overlappable so that we can use the TH to derive this for things.  Otherwise this covers all things!
instance {-# OVERLAPPABLE #-} (Show a, Read a,Data a)=>Builder BuilderEx a where
  buildM = simpleBuilder 

f = maybe (putStrLn "built Nothing") (putStrLn . show) 


main::IO ()
main = do

  putStrLn "Given:\ndata TestSum = A | B Int | C Char Int | D Char Int Bool deriving (Show)\ndata TestNested = Nested Int String TestSum deriving (Show)"
  putStrLn "Build a TestSum from a given value (=C 'a' 12)"
  builtIO (buildM Nothing (Just $ C 'a' 12)) >>= f
  putStrLn "Build a TestNested from Nothing"
  builtIO (buildM Nothing (Nothing :: Maybe TestNested)) >>= f
  putStrLn "Build a TestSum from a value (=TestNested 12 \"Hello\" (D 'a' 2 True)"
  builtIO (buildM Nothing (Just $ Nested 12 "Hello" (D 'a' 2 True))) >>= f
  

{-
-- You need base case builders.  How to build basic types
simpleBuilder::(Show a, Read a)=>Maybe a->MFM IO Identity a 
simpleBuilder Nothing = putStr "Enter: " >> hFlush stdout >> ((fmap Identity) (readMaybe <$> getLine))
simpleBuilder (Just a) = putStr ("Enter (was=" ++ show a ++ "):") >> hFlush stdout >> ((fmap Identity) (readMaybe <$> getLine))

-- This is overlappable so that we can use the TH to derive this for things.  Otherwise this covers all things!
instance {-# OVERLAPPABLE #-} (Show a, Read a)=>Builder IO Identity a where
  buildM = simpleBuilder 

-- you need a way of handling multiple contructors for a type
instance ApplicativeLike f=>ConSummable IO f where
  sumF conData = case (length conData) of
    0 -> putStrLn "Zero length list of constructors given to sumF" >> return (alInject Nothing)
    1 -> let (_,SumConstructor _ bldr) = head conData in bldr
    _ -> do
      let toName (cid,SumConstructor isDflt _) = cid ++ if (isDflt) then "*" else ""
          prompt = "Type has multiple constructors.  Please choose (" ++ (intercalate "," $ map toName conData) ++ "): "
      putStr prompt
      hFlush stdout
      cid <- getLine
      let mCid = lookup cid conData
      case mCid of
        Nothing -> putStrLn (cid ++ " unrecognized constructor!") >> return (alInject Nothing)
        Just (SumConstructor _ b) -> b

-}

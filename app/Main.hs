{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-} --just for demo.  Allows polymorphic result printer
module Main where

import DataBuilderTH
import Control.Monad (ap)
import Language.Haskell.TH
--import Language.Haskell.TH.Lift
import Data.Functor.Identity
import Data.List (intercalate)
import Text.Read (readMaybe)
import System.IO (hFlush,stdout)

data TestNull = Null deriving (Show)
data TestOne = One Int deriving (Show)
data TestTwo = Two Int String deriving (Show)
data TestSum = A | B Int | C Char Int | D Char Int Bool deriving (Show)
data TestRecord = TestR { intF::Int, stringF::String } deriving (Show)
data TestNested = Nested Int String TestSum deriving (Show)

deriveBuild ''TestNull
deriveBuild ''TestOne
deriveBuild ''TestTwo
deriveBuild ''TestSum
deriveBuild ''TestRecord
deriveBuild ''TestNested

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


f = maybe (putStrLn "built Nothing") (putStrLn . show) 

main::IO ()
main = do

  putStrLn "Given:\ndata TestSum = A | B Int | C Char Int | D Char Int Bool deriving (Show)\ndata TestNested = Nested Int String TestSum deriving (Show)"
  putStrLn "Build a TestSum from a given value (=C 'a' 12)"
  runIdentity <$> buildM (Just $ C 'a' 12) >>= f
  putStrLn "Build a TestNested from Nothing"
  runIdentity <$> buildM (Nothing :: Maybe TestNested) >>= f
  putStrLn "Build a TestSum from a value (=TestNested 12 \"Hello\" (D 'a' 2 True)"
  runIdentity <$> buildM (Just $ Nested 12 "Hello" (D 'a' 2 True)) >>= f
  

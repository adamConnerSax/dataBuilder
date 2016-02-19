{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import DataBuilderTH
import Control.Monad (ap)
import Language.Haskell.TH
--import Language.Haskell.TH.Lift
import Data.Functor.Identity
import Data.List (intercalate)
import Text.Read (readMaybe)


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
simpleBuilder Nothing = putStr "Enter: " >> ((fmap Identity) (readMaybe <$> getLine))
simpleBuilder (Just a) = putStr ("Enter (was=" ++ show a ++ "):") >> ((fmap Identity) (readMaybe <$> getLine))

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
      cid <- getLine
      let mCid = lookup cid conData
      case mCid of
        Nothing -> putStrLn (cid ++ " unrecognized constructor!") >> return (alInject Nothing)
        Just (SumConstructor _ b) -> b


main::IO ()
main = do
  mSum <- runIdentity <$> buildM (Just $ C 'a' 12)
  maybe (putStrLn "built Nothing") (putStrLn . show) mSum
  mNested <- runIdentity <$> buildM (Nothing :: Maybe TestNested)
  maybe (putStrLn "built Nothing") (putStrLn . show) mNested

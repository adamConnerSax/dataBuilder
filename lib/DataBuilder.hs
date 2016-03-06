{-|
Module: DataBuilder.InternalTypes
Description: Types and TypeClasses for DataBuilder package
Copyright: (c) Adam Conner-Sax 2016
License: BSD3
Maintainer: adam_conner_sax@yahoo.com

The DataBuilder package provides a framework for constructing builders and/or editors of data structures from just the structures and something which is a slightly enhanced applicative functor representing the way builders are put together.

Some examples (each has a working version in the source):

Using the optparse-applicative Parser type as the applicative functor leads to a simple way of directly building applicative parsers for a data structure (see ???).    

Using an HTML form monad (e.g., Yesod's AForm) as our applicative functor, we can create forms from data types directly (see ????).

Using an Reflex-FRP widget containing a dynamic value (not an actual applicative functor), we can create forms that update the value as the fields are updated (see ??).  

To use, create an instance of "Buildable" for your f :: * -> *. This will almost always be an applicative functor but it need not be.
If your f is an applicative, Buildable is basically trivial to derive.  You will need two extra methods, bFail (to handle failure at various levels of data building) and bSum (to handle sum-types).

Once you have an instance of Buildable, you can instantiate your builders.  You may use Template Haskell via the "deriveBuilder" TH function or you can make a generic instance.  The latter requires that your types instantiate GHC.Generic as well as Generics.SOP.Generic and Generics.SOP.HasDatatypeInfo.  Generic can be derived as part of your deriving clause if you enable the DeriveGeneric extension and the other two can be instantiated via empty instance declarations.  Please see examples for more information.


This file just exports the relevant types and the Template Haskell and Generic implementations of the instances.
-}
module DataBuilder
       (
           module DataBuilder.Types
         , module DataBuilder.TH
         , module DataBuilder.GenericSOP
       ) where

import DataBuilder.Types
import DataBuilder.TH
import DataBuilder.GenericSOP

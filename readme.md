DataBuilder

Prototype template and generics-sop to applicatively build a value of type a from an existing value of a (or Nothing::Maybe a) using a typeclass.  Provide base builders via class instances (for simple types, like Int, String, etc.), and a function to handle sum types, and the derived class will create a builder for types built of sums and products of types with builders.  Something like Aeson's To/FromJSON but with a more general target.

For any given type, you may make your own builder (by writing your own instance of Builder f g a) if the template derived behavior is not appropriate.


Types:

```
type FieldName = String --Record selectors
type ConName = String --Constructor names

data MDWrapped f g a = MDWrapped { hasDefault::Bool, metadata::(ConName, Maybe FieldName), value::f a }

class Applicative f=>Buildable f where
  bFail::String->f a -- handle errors...
  bSum::[MDWrapped f g a]->f a -- this only gets called if the list length (actual sum type).  If you have 0 or > 1 but are missing a constructor, bFail is called.

class Builder f a where
  buildM::Buildable f g=>g->Maybe a-> f a


```

To use TH to derive instances you need to make a specific TH for your Builder type.  See examples.
To use generics-sop, you need to derive the Generics.SOP.Generic and Generics.SOP.HasDatatypeInfo classes for your type first.  Those are auto-derivable if you have a GHC.Generics.Generic instance. E.g.,

```
{-# LANGUAGE DeriveGeneric #-}
import qualified GHC.Generics as G
import qualified DataBuilder as DB --DataBuilder exports the Generics.SOP classes for convenience

data A = A Int String deriving (G.Generic)

instance DB.Generic A
instance DB.HasDatatypeInfo A
instance Buildable YourFunctor => Builder YourFunctor A

```


Two examples:

1. In "optionParse", an optparse-applicative builder.  Pretty bare bones but it could be expanded.

2. In "app", a contrived example; we build interactively via readMaybe and putStr/getLn. But it shows the complex builder being built up from the simpler ones and a non-trivial sum-type case.


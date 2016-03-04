DataBuilder

Prototype template haskell to applicatively build a value of type a from an existing value of a (or Nothing::Maybe a) using a typeclass.  Provide base builders via class instances (for simple types, like Int, String, etc.), and a function to handle sum types, and the derived class will create a builder for types built of sums and products of types with builders.  Something like Aeson's FromJSON.

Extra, runtime specified, options may be passed to each builder by creating a type to wrap the type-level metadata and deriving HasMetadata.  See the optionParser example for more details.

For any given type, you may make your own builder (by writing your own instance of Builder f g a) if the template derived behavior is not appropriate.

Types:

```
type TypeName = String
type FieldName = String
type ConName = String
data Metadata = Metadata { typeName::TypeName, conName::Maybe ConName, fieldName::Maybe FieldName}

class HasMetadata a where
  getMetadata::a->Metadata
  setMetadata::Metadata->a->a

data MDWrapped f g a = MDWrapped { hasDefault::Bool, metadataHolder::g, value::f a }

instance HasMetadata g=>HasMetadata (MDWrapped f g a) where
  getMetadata = getMetadata . metadataHolder 
  setMetadata md (MDWrapped hd mdh v) = let mdh' = setMetadata md mdh in MDWrapped hd mdh' v


class Buildable f g | f->g where
  -- inject and apply are exactly Applicative, inject=pure and apply=(<*>). 
  bInject::a -> f a
  bApply::f (a->b) -> f a -> f b
  bFail::String->f a -- handle errors...
  bSum::[MDWrapped f g a]->f a -- this only gets called if you have > 1 (actual sum type).  If you have 0 or > 1 but are missing a constructor, bFail is called.

class Builder f g a where
  buildM::(HasMetadata g,Buildable f g)=>g->Maybe a-> f a


```

Two examples:

1. In "optionParse", an optparse-applicative builder.  Pretty bare bones but it could be expanded.

2. In "app", a contrived example; we build interactively via readMaybe and putStr/getLn. But it shows the complex builder being built up from the simpler ones and a non-trivial sum-type case.


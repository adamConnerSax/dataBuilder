DataBuilder

Prototype template haskell to monadically build a value of type a from an existing value of a (or Nothing::Maybe a) using a typeclass.  Provide base builders via class instances (for simple types, like Int, String, etc.), and a function to handle sum types, and the derived class will create a builder for types built of sums and products of types with builders.  Something like Aeson's FromJSON.

For any given type, you may make your own builder (by writing your won instance of Builder f) if the template derived behavior is not appropriate.

Types:

```
type TypeName = String
type FieldName = String
type ConName = String
data Metadata = Metadata { typeName::TypeName, conName::Maybe ConName, fieldName::Maybe FieldName}

data MDWrapped f a = MDWrapped { hasDefault::Bool, metadata::Metadata, value::f a } --Exposed so data is available for sum type handling and hand derived instances of Builder

class Buildable f where
  -- inject and apply are exactly Applicative, inject=pure and apply=(<*>). 
  bInject::a -> f a
  bApply::f (a->b) -> f a -> f b
  bSum::[MDWrapped f a]->f a

class Builder f a where
  buildM::Buildable f=>Metadata->Maybe a-> f a

```

Two examples:

1. In "optionParse", an optparse-applicative builder.  Pretty bare bones but it could be expanded.

2. In "app", a contrived example; we build interactively via readMaybe and putStr/getLn. But it shows the complex builder being built up from the simpler ones and a non-trivial sum-type case.


DataBuilder

Prototype template haskell to monadically build a value of type a from an existing value of a (or Nothing::Maybe a) using a typeclass.  Provide base builders (for simple types, like Int, String, etc.), and a function to handle sum types, and the derived class will create a builder for a more complicated class.

Types:

given a base Monad m (IO in the example)

```

type MFM m f a = m (f (Maybe a))

class ApplicativeLike f where
  alInject::a->f a
  alApply::f (x->y) -> f x -> f y

instance Applicative f=>ApplicativeLike f where
  alInject = pure
  alApply = (<*>)

class FM m f where
  injectMfM::a->MFM m f a
  applyMfM:::MFM m f (x->y)->MFM m f x->MFM m f y
  
instance (Monad m, ApplicativeLike f)=>FM m f where
  injectMfM x = return $ (alInject (Just x))
  applyMfM mfmFxy mfmX = do
    fmFXY <- mfmFxy
    fmX <- mfmX
    return $ alApply (alApply (alInject (<*>)) fmFXY) fmX

type ConId = String
data SumConstructor m f a = SumConstructor { dflt::Bool, builder::MFM m f a }

class ConSummable m f where
  sumF::[(ConId,SumConstructor m f a)] -> MFM m f a 

class Builder m f a where
  buildM::(Monad m,ApplicativeLike f,ConSummable m f)=>Maybe a->MFM m f a
```

In most cases "f" will be Identity and can be ignored.

In the given--extremely contrived!--example, we build interactively via readMaybe and putStr/getLn. But it shows the complex builder being built up from the simpler ones.
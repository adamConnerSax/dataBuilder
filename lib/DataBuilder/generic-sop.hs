ci2name::ConstructorInfo xs -> DatatypeName
ci2name (Constructor n) = n
ci2name (Infix n _ _) = n
ci2name (Record n _) = n

ci2RecordNames::ConstructorInfo xs -> Maybe (NP FieldInfo xs)
ci2RecordNames (Record _ fi) = Just fi
ci2RecordNames _ = Nothing

--newMetadata::HasMetadata g=>g->DatatypeName->ConstructorInfo xs->g->g
--newMetadata tn ci mdh = setMetadata (Metadata tn (Just $ ci2name ci) (fieldName . getMetadata mdh))

addFieldName::forall a g.HasMetadata g=>FieldInfo a->g->g
addFieldName fi mdh = let (FieldInfo fn) = fi in setMetaData (getMetaData mdh { fieldName = Just fn }) mdh

-- SListN can prob be SListI ?
buildBlank'::forall f g h xs.(Buildable f g, All (Builder f g) xs, HasMetadata g, SListI xs)=>
             g->DatatypeName->ConstructorInfo xs->NP f xs
buildBlank' mdh tn ci = 
    let mdhBase = setMetaData (MetaData tn (Just $ ci2name ci) Nothing) mdh
        fieldNames = ci2RecordNames ci
    in case fieldNames of 
         Nothing -> hcpure (Proxy :: Proxy (Builder f g)) (buildM mdhBase Nothing)
         Just fns -> 
             let builder::FieldInfo a -> f a
                 builder fi = buildM (addFieldName fi mdhBase) Nothing
             in hcliftA (Proxy :: Proxy (Builder f g)) builder fns

-- do we need hcliftA?  And a constraint on application of BuilderFA?  I don't think so, you can 
-- BuilderFA any functor.  The constraint applies to the unFA operation. I think.
buildBlank::forall f g h xs.(Buildable f g, All (Builder f g) xs, HasMetadata g, SListI xs)=>
            g->DataTypeName->ConstructorInfo xs->f (NP I xs)
buildBlank mdh tn ci = unFA . hsequence $ hliftA BuilderFA (buildBlank' mdh tn ci) 


buildBlanks::forall f g h xs a.(Buidable f g, All2 (Builder f g), HasMetadata g,
                                HasDatatypeInfo a,SListIN h xs) =>
             g->a->POP f xss
buildBlanks mdh a = 
    let dtInfo = datatypeInfo (Proxy :: Proxy a)
    in case dtInfo of
         ADT _ tn cs -> buildBlanks' mdh tn cs
         Newtype _ tn c -> buildBlanks' mdh tn (c :* Nil)

buildBlanks'::forall f g h xs a.(Buidable f g, All2 (Builder f g), HasMetadata g,
                                HasDatatypeInfo a,SListIN h xs) =>
              g->NP ConstructorInfo xss->POP f xss
buildBlanks' mdh cs = h

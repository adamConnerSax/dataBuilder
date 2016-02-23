module DataBuilder.Generics where

import Generics.Eot
import Generics.Generic
import DataBuilder.InternalTypes

{-
class EotBuilder f g eot where
  eotBuildM :: (HasMetadata g, Buildable f g)=>Int --constructor number for Eot
            ->g --Metadata carrier
            ->Maybe eot --input to the builder
            ->f eot --result

instance (EotBuilder this, EotBuilder next) => EotBuilder (Either this next) where
  eotBuilder n mdh Nothing =  
-}


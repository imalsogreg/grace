{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

{-| This module exists primarily to avoid a name clash with constructors of the
    same name in the "Grace.Type" module
-}
module Grace.Domain
    ( -- * Domain
      Domain(..)
    ) where

import GHC.Generics (Generic)
import Grace.Pretty (Pretty(..), builtin)
import Language.Haskell.TH.Syntax (Lift)

-- | The domain over which a @forall@ is quantified
data Domain
    = Type
    -- ^ @forall (a : Type) . …@
    | Fields
    -- ^ @forall (a : Fields) . …@
    | Alternatives
    -- ^ @forall (a : Alternatives) . …@
    | TensorShape
    -- ^ forall (a : Size) . …@
    deriving stock (Eq, Generic, Lift, Show)

instance Pretty Domain where
    pretty Type         = builtin "Type"
    pretty Fields       = builtin "Fields"
    pretty Alternatives = builtin "Alternatives"
    pretty TensorShape  = builtin "TensorShape"

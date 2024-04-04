{-# LANGUAGE UndecidableInstances #-} -- for IsEnum

-- | Structural assertions on generic data representation.

module Generic.Data.Rep.Assert where

import GHC.Generics
import GHC.TypeError
import GHC.TypeLits ( type Symbol )
import Data.Kind ( type Constraint )

type GAssertError :: k -> Symbol -> Constraint
type GAssertError a msg = TypeError
         ('Text "Assertion on generic representation failed for type: "
    :<>: 'ShowType a
    :$$: 'Text "Message: " :<>: 'Text msg)

type family StripD1 a where StripD1 (D1 _ a) = a

-- | Type is not void i.e. has at least one constructor.
type GAssertNotVoid a = Assert (IsNotVoid (StripD1 (Rep a)))
    (GAssertError a "not non-void type (>=1 constructor)")
type family IsNotVoid a where
    IsNotVoid V1  = False
    IsNotVoid _ = True

-- | Type is not a sum type i.e. has at most one constructor.
--
-- Permits void types.
type GAssertNotSum a = Assert (IsNotSum (StripD1 (Rep a)))
    (GAssertError a "not non-sum type (1 constructor)")
type family IsNotSum a where
    IsNotSum (_ :+: _) = False
    IsNotSum _ = True

-- | Type is a sum type i.e. has >=2 constructors.
--
-- Permits void types.
type GAssertSum a = Assert (IsSum (StripD1 (Rep a)))
    (GAssertError a "not sum type (>=2 constructors)")
type family IsSum a where
    IsSum (C1 _ _) = False
    IsSum _ = True

-- | Type has only empty constructors.
--
-- Permits void types.
type GAssertEnum a = Assert (IsEnum (StripD1 (Rep a)))
    (GAssertError a "not enum type (all empty constructors)")
type family IsEnum a where
    IsEnum V1 = True
    IsEnum (C1 _ a) = ConsIsEmpty a
    IsEnum (l :+: r) = IsEnum l `And` IsEnum r

type family ConsIsEmpty a where
    ConsIsEmpty U1 = True
    ConsIsEmpty _  = False

-- | Type level boolean AND.
type family And l r where
    And True True = True
    And _    _    = False

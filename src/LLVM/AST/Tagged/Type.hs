{-# LANGUAGE UndecidableInstances #-}

-- | This module provides a type-safe variant of "LLVM.AST.Type".
module LLVM.AST.Tagged.Type
( FloatingPointType'(..)
, Type(..)

-- * Type aliases
, Void
, Ptr

-- ** Integer types
, I1
, I8
, I16
, I32
, I64
, I128

-- ** Floating point types
, Half
, Float'
, Double'
, FP128
, X86_FP80
, PPC_FP128

-- * Constraints
, NonVoid
, NonAggregate
, BitSizeOfFP
, BitSizeOf
, ValueAt
) where

import GHC.Exts (Constraint)

import LLVM.AST.Type qualified as NonTagged
import LLVM.AST.Tagged.AddrSpace
import LLVM.AST.TypeLevel

-- | Redefinition of 'NonTagged.FloatingPointType'. Only difference is appending @'@ to the type
-- name so it doesn't conflict with the 'FloatingPointType' constructor.
data FloatingPointType'
  = HalfFP      -- ^ 16-bit floating point value
  | FloatFP     -- ^ 32-bit floating point value
  | DoubleFP    -- ^ 64-bit floating point value
  | FP128FP     -- ^ 128-bit floating point value (112-bit mantissa)
  | X86_FP80FP  -- ^ 80-bit floating point value (X87)
  | PPC_FP128FP -- ^ 128-bit floating point value (two 64-bits)

-- | Redefinition of 'NonTagged.Type' for use at the type-level.
--
-- The redefinition is necessary since we must use 'Nat' rather than other integer types that can't
-- be used at the type-level. Additionally, we omit 'NonTagged.NamedTypeReference' since there is no
-- way to verify properties of the type of a named reference at the type-level. Since pointers in
-- LLVM are now opaque, recursive types are no longer possible, meaning that named types are no
-- longer necessary.
data Type
  = VoidType
  | IntegerType Nat -- ^ width in bits
  | PointerType AddrSpace'
  | FloatingPointType FloatingPointType'

  -- | we do not support varargs in the typed represenation
  | FunctionType
      Type   -- ^ return type
      [Type] -- ^ parameter types

  | VectorType
      Nat    -- ^ element count
      Type   -- ^ element type

  | StructureType
      Bool   -- ^ is packed
      [Type] -- ^ element types

  | ArrayType
      Nat    -- ^ element count
      Type   -- ^ element type

  | MetadataType
  | TokenType
  | LabelType

type Void = VoidType
type Ptr  = PointerType (AddrSpace 0)

type I1   = IntegerType 1
type I8   = IntegerType 8
type I16  = IntegerType 16
type I32  = IntegerType 32
type I64  = IntegerType 64
type I128 = IntegerType 128

type Half      = FloatingPointType HalfFP
type Float'    = FloatingPointType FloatFP
type Double'   = FloatingPointType DoubleFP
type FP128     = FloatingPointType FP128FP
type X86_FP80  = FloatingPointType X86_FP80FP
type PPC_FP128 = FloatingPointType PPC_FP128FP

-- | Ensures a type is not void
type family NonVoid (t :: Type) :: Constraint where
    NonVoid VoidType = TypeError (Text "Type must not be void")
    NonVoid t         = ()

-- | A non-aggregate, non-vector type. Basically, everything that can
-- be bitcaste’d into each other.
type family NonAggregate (t :: Type) :: Constraint where
    NonAggregate (IntegerType _)       = ()
    NonAggregate (FloatingPointType _) = ()
    NonAggregate (VectorType _ _ )     = ()
    NonAggregate t = TypeError (ShowType t :<>: Text " is aggregate")

-- | Bit widths of the given floating point type
type family BitSizeOfFP (t :: FloatingPointType') :: Nat where
    BitSizeOfFP HalfFP      = 16
    BitSizeOfFP FloatFP     = 32
    BitSizeOfFP DoubleFP    = 64
    BitSizeOfFP FP128FP     = 128
    BitSizeOfFP X86_FP80FP  = 80
    BitSizeOfFP PPC_FP128FP = 128

-- | Bit widths of this nonaggregate type
type family BitSizeOf (t :: Type) :: Nat where
    BitSizeOf (IntegerType w)         = w
    BitSizeOf (FloatingPointType fpf) = BitSizeOfFP fpf
    BitSizeOf (VectorType n t)        = n * BitSizeOf t
    BitSizeOf t = TypeError (ShowType t :<>: Text " is aggregate")

-- | Get the result of indexing into an aggregate type.
type family ValueAt (t :: Type) (as :: [Nat]) :: Type where
    ValueAt t '[] = t
    ValueAt (StructureType _ ts) (n : as) = ValueAt (Nth ts n) as
    ValueAt (ArrayType _ t2)     (_ : as) = ValueAt t2 as
    ValueAt t _ = TypeError (Text "Cannot index into non-aggregate type " :$$: ShowType t)

type instance Value FloatingPointType' = NonTagged.FloatingPointType
instance Known HalfFP      where knownVal = NonTagged.HalfFP
instance Known FloatFP     where knownVal = NonTagged.FloatFP
instance Known DoubleFP    where knownVal = NonTagged.DoubleFP
instance Known FP128FP     where knownVal = NonTagged.FP128FP
instance Known X86_FP80FP  where knownVal = NonTagged.X86_FP80FP
instance Known PPC_FP128FP where knownVal = NonTagged.PPC_FP128FP

type instance Value Type = NonTagged.Type
instance Known VoidType where
  knownVal = NonTagged.VoidType
instance Known n => Known (IntegerType n) where
  knownVal = NonTagged.IntegerType (word32Val @n)
instance Known as => Known (PointerType as) where
  knownVal = NonTagged.PointerType (val @as)
instance Known fpf => Known (FloatingPointType fpf) where
  knownVal = NonTagged.FloatingPointType (val @fpf)
instance (Known ret, Known args) => Known (FunctionType ret args) where
  knownVal = NonTagged.FunctionType (val @ret) (val @args) False
instance (Known n, Known t) => Known (VectorType n t) where
  knownVal = NonTagged.VectorType (word32Val @n) (val @t)
instance (Known packed, Known elts) => Known (StructureType packed elts) where
  knownVal = NonTagged.StructureType (val @packed) (val @elts)
instance (Known n, Known t) => Known (ArrayType n t) where
  knownVal = NonTagged.ArrayType (word64Val @n) (val @t)
instance Known MetadataType where
  knownVal = NonTagged.MetadataType
instance Known TokenType where
  knownVal = NonTagged.TokenType
instance Known LabelType where
  knownVal = NonTagged.LabelType
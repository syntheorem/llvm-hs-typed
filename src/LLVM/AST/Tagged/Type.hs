{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

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
, ValidType
, FirstClassType
, SizedType
, VectorElementType
, ReturnType
, AtomicType
, ZeroInitializable
, BitCastable
, IntOrIntVector
, FloatOrFloatVector
, PtrOrPtrVector

-- * Type-level functions
, BitSizeOf
, BitSizeOfFP
, ValueAt
, VectorLength
, ReplaceVectorType
) where

import Data.Type.Ord (type (<?))
import GHC.TypeLits (type (^), Log2)

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

type MaxIntegerWidth = 0x800000 -- max bitwidth is 2^23 according to LLVM language ref
type MaxVectorLength = 0xFFFFFFFF -- vector length is stored in Word32
type MaxArrayLength  = 0xFFFFFFFFFFFFFFFF -- array length is stored in Word64
type MaxAddrSpace    = 0xFFFFFFFF -- address space is stored in Word32

-- | Constraint that ensures an LLVM type is well-formed.
type family ValidType (t :: Type) :: Constraint where
  ValidType VoidType                     = ()
  ValidType (IntegerType w)              = (0 < w, w <= MaxIntegerWidth)
  ValidType (PointerType (AddrSpace as)) = (as <= MaxAddrSpace)
  ValidType (FloatingPointType _)        = ()
  ValidType (FunctionType ret params)    = (ReturnType ret, AllSatisfy FirstClassType params)
  ValidType (VectorType n t)             = (0 < n, n <= MaxVectorLength, VectorElementType t)
  ValidType (StructureType _ ts)         = (AllSatisfy SizedType ts)
  ValidType (ArrayType n t)              = (n < MaxArrayLength, SizedType t)
  ValidType MetadataType                 = ()
  ValidType TokenType                    = ()
  ValidType LabelType                    = ()

-- | Types that can be used in an LLVM register.
-- This includes all types except void and function types.
class ValidType t => FirstClassType t
instance FirstClassType MetadataType
instance FirstClassType TokenType
instance FirstClassType LabelType
instance FirstClassType (FloatingPointType fpt)
instance ValidType (IntegerType w)      => FirstClassType (IntegerType w)
instance ValidType (PointerType as)     => FirstClassType (PointerType as)
instance ValidType (VectorType n t)     => FirstClassType (VectorType n t)
instance ValidType (StructureType p ts) => FirstClassType (StructureType p ts)
instance ValidType (ArrayType n t)      => FirstClassType (ArrayType n t)

-- | Types with a size that can be used in array or structure types.
-- This includes all first-class types except metadata, label, and token.
class FirstClassType t => SizedType t
instance SizedType (FloatingPointType fpt)
instance ValidType (IntegerType w)      => SizedType (IntegerType w)
instance ValidType (PointerType as)     => SizedType (PointerType as)
instance ValidType (VectorType n t)     => SizedType (VectorType n t)
instance ValidType (StructureType p ts) => SizedType (StructureType p ts)
instance ValidType (ArrayType n t)      => SizedType (ArrayType n t)

-- | Types that can be used as an element of a vector type.
-- This includes all floating point, integer, and pointer types.
class SizedType t => VectorElementType t
instance VectorElementType (FloatingPointType fpt)
instance ValidType (IntegerType w)  => VectorElementType (IntegerType w)
instance ValidType (PointerType as) => VectorElementType (PointerType as)

-- | Types that can be used as the return type of a function.
-- Includes void and all first-class types other than metadata and label.
class ValidType t => ReturnType t
instance ReturnType TokenType -- only usable by intrinsics
instance ReturnType (FloatingPointType fpt)
instance ValidType (IntegerType w)      => ReturnType (IntegerType w)
instance ValidType (PointerType as)     => ReturnType (PointerType as)
instance ValidType (VectorType n t)     => ReturnType (VectorType n t)
instance ValidType (StructureType p ts) => ReturnType (StructureType p ts)
instance ValidType (ArrayType n t)      => ReturnType (ArrayType n t)

-- | Types that can be used in an atomic operations.
-- Includes pointer, integer, and floating point types with a power-of-two size.
class SizedType t => AtomicType t
instance ValidType (PointerType as) => AtomicType (PointerType as)
instance (ValidType (IntegerType w), AtomicBitSize w) => AtomicType (IntegerType w)
instance AtomicBitSize (BitSizeOfFP fpt) => AtomicType (FloatingPointType fpt)

-- | Ensures a given bit width is valid for atomic operations.
--
-- LLVM requires atomic operations to use types whose bit width is a power of 2 that is at least 8.
type family AtomicBitSize (s :: Nat) :: Constraint where
  AtomicBitSize 0 = TypeError (Text "zero is not a power of two")
  AtomicBitSize s = (8 <= s, s ~ (2 ^ Log2 s))

-- | Types that can use a @zeroinitialize@ constant.
--
-- The LLVM language reference says that any type can be used with @zeroinitialize@, but @llvm-hs@
-- only allows it to be used with array, structure, and vector types.
class SizedType t => ZeroInitializable t
instance ValidType (VectorType n t)     => ZeroInitializable (VectorType n t)
instance ValidType (StructureType p ts) => ZeroInitializable (StructureType p ts)
instance ValidType (ArrayType n t)      => ZeroInitializable (ArrayType n t)

-- | Types that can be bitcasted.
-- Includes all integer and floating point types, and vectors of those types.
class SizedType t => BitCastable t
instance ValidType (IntegerType w)                        => BitCastable (IntegerType w)
instance ValidType (FloatingPointType fpt)                => BitCastable (FloatingPointType fpt)
instance ValidType (VectorType n (IntegerType w))         => BitCastable (VectorType n (IntegerType w))
instance ValidType (VectorType n (FloatingPointType fpt)) => BitCastable (VectorType n (FloatingPointType fpt))

-- | A type that is either 'IntegerType' or 'VectorType' with 'IntegerType' elements.
class BitCastable t => IntOrIntVector (len :: Maybe Nat) (width :: Nat) (t :: Type) | t -> width len
instance ValidType (IntegerType w) =>
  IntOrIntVector Nothing w (IntegerType w)
instance ValidType (VectorType n (IntegerType w)) =>
  IntOrIntVector (Just n) w (VectorType n (IntegerType w))

-- | A type that is either 'FloatingPointType' or 'VectorType' with 'FloatingPointType' elements.
class BitCastable t => FloatOrFloatVector (len :: Maybe Nat) (fpt :: FloatingPointType') (t :: Type) | t -> fpt len
instance ValidType (FloatingPointType fpt) =>
  FloatOrFloatVector Nothing fpt (FloatingPointType fpt)
instance ValidType (VectorType n (FloatingPointType fpt)) =>
  FloatOrFloatVector (Just n) fpt (VectorType n (FloatingPointType fpt))

-- | A type that is either 'PointerType' or 'VectorType' with 'PointerType' elements.
class SizedType t => PtrOrPtrVector (len :: Maybe Nat) (as :: AddrSpace') (t :: Type) | t -> as len
instance ValidType (PointerType as) =>
  PtrOrPtrVector Nothing as (PointerType as)
instance ValidType (VectorType n (PointerType as)) =>
  PtrOrPtrVector (Just n) as (VectorType n (PointerType as))

-- | Size in bits of a Bitcastable type.
type family BitSizeOf (t :: Type) :: Nat where
  BitSizeOf (IntegerType w)         = w
  BitSizeOf (FloatingPointType fpf) = BitSizeOfFP fpf
  BitSizeOf (VectorType n t)        = n * BitSizeOf t
  BitSizeOf t = TypeError (ShowType t :<>: Text " is aggregate")

-- | Bit width of the given floating point type.
type family BitSizeOfFP (t :: FloatingPointType') :: Nat where
  BitSizeOfFP HalfFP      = 16
  BitSizeOfFP FloatFP     = 32
  BitSizeOfFP DoubleFP    = 64
  BitSizeOfFP FP128FP     = 128
  BitSizeOfFP X86_FP80FP  = 80
  BitSizeOfFP PPC_FP128FP = 128

-- | Get the result of indexing into an aggregate type.
type family ValueAt (t :: Type) (idxs :: [Nat]) :: Type where
  ValueAt t '[] = t
  ValueAt (StructureType _ ts) (i : idxs) = ValueAt (Nth ts i) idxs
  ValueAt (ArrayType n t)      (i : idxs)
    = IfThenElse (i <? n)
        (ValueAt t idxs)
        (TypeError (Text "out-of-bounds index " :<>: ShowType i))
  ValueAt t _ = TypeError (Text "index into non-aggregate type: " :<>: ShowType t)

-- | If @t@ is a 'VectorType', result in @Just n@, where @n@ is the length of the vector.
type family VectorLength (t :: Type) :: Maybe Nat where
  VectorLength (VectorType n _) = Just n
  VectorLength _                = Nothing

-- | If @vt@ is a 'VectorType', replace its element type with @t@. Otherwise, return @t@.
type family ReplaceVectorType (vt :: Type) (t :: Type) where
  ReplaceVectorType (VectorType n _) t = VectorType n t
  ReplaceVectorType _                t = t

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
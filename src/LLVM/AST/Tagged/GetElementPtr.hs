{-# LANGUAGE UndecidableInstances #-}

-- | Types and functions for working with the getelementptr instruction.
--
-- Needed so we can validate GEP indicies at the type-level. Indexing into a struct type requires a
-- statically-known index, but an array or vector index can be an arbitrary operand. In this module,
-- we abstract over the type of this operand, since it can be an 'Operand' or 'Constant'.
module LLVM.AST.Tagged.GetElementPtr
( GEP_Indices
, GEP_IndicesValid

-- * Construction
, IsGEP_Index
, knownIx
, (.:)

-- * Internal helpers
, gepIndicesToOperands
) where

import LLVM.AST.TypeLevel
import LLVM.AST.Tagged.Tag
import LLVM.AST.Tagged.Type
import LLVM.AST.Constant (Constant)
import LLVM.AST.Constant qualified as Constant
import LLVM.AST.Operand (Operand(ConstantOperand))
import LLVM.AST.Operand qualified as Operand

import Data.Type.Ord (type (<?))
import Data.Kind (Constraint)
import Data.Data (Proxy(Proxy))
import Data.Word (Word32)

-- | List of indices used for a GEP instruction.
data GEP_Indices (vecSize :: Maybe Nat) opd (idxs :: [Maybe Nat]) where
  -- | Statically known index (required for struct element indexing).
  GEP_KnownIndex
    :: Known i
    => GEP_Indices vecSize opd idxs
    -> GEP_Indices vecSize opd (Just i : idxs)

  -- | Index not known at compile time (only allowed for array or vector indexing).
  GEP_UnknownIndex
    :: Known w
    => opd ::: IntegerType w
    -> GEP_Indices Nothing opd idxs
    -> GEP_Indices Nothing opd (Nothing : idxs)

  -- | Vector of indices not known at compile time.
  GEP_UnknownIndexVector
    :: Known w
    => opd ::: VectorType n (IntegerType w)
    -> GEP_Indices (Just n) opd idxs
    -> GEP_Indices (Just n) opd (Nothing : idxs)

  -- | List terminator.
  GEP_Nil :: GEP_Indices vecSize opd '[]

-- | Constraint that enforces the validity of the GEP indicies for a given type.
type family GEP_IndicesValid t (idxs :: [Maybe Nat]) :: Constraint where
  GEP_IndicesValid t '[] = ()
  -- First index is a pointer offset and doesn't change the type
  GEP_IndicesValid t (_ : idxs) = GEP_IndicesValid' t idxs

type family GEP_IndicesValid' t (idxs :: [Maybe Nat]) :: Constraint where
  GEP_IndicesValid' _ '[] = ()
  -- Structure types require a known index ('Nth' will ensure the index is in bounds).
  GEP_IndicesValid' (StructureType _ ts) (Just i : idxs) = GEP_IndicesValid' (Nth ts i) idxs
  GEP_IndicesValid' (StructureType _ _)  (Nothing : _)   = TypeError (Text "unknown index for structure type")
  -- Even for a constant index, LLVM doesn't do bounds checking on a GEP into an array or vector type.
  GEP_IndicesValid' (ArrayType  n t) (_ : idxs) = GEP_IndicesValid' t idxs
  GEP_IndicesValid' (VectorType n t) (_ : idxs) = GEP_IndicesValid' t idxs
  GEP_IndicesValid' t _ = TypeError (Text "invalid getelementptr index into type " :<>: ShowType t)

class IsGEP_Index (vecSize :: Maybe Nat) opd (idx :: Maybe Nat) a | a -> idx where
  toGEP_Indices :: a -> GEP_Indices vecSize opd idxs -> GEP_Indices vecSize opd (idx : idxs)

instance Known i => IsGEP_Index vecSize opd (Just i) (Proxy i) where
  toGEP_Indices _ = GEP_KnownIndex

instance (FirstClassType (IntegerType w), Known w) =>
  IsGEP_Index Nothing opd Nothing (opd ::: IntegerType w) where
    toGEP_Indices = GEP_UnknownIndex

instance (FirstClassType (VectorType n (IntegerType w)), Known w) =>
  IsGEP_Index (Just n) opd Nothing (opd ::: VectorType n (IntegerType w)) where
    toGEP_Indices = GEP_UnknownIndexVector

-- | Function to help construct a known index when building a list of GEP indicies.
knownIx :: forall (i :: Nat). Proxy i
knownIx = Proxy

-- | Operator to help construct a list of GEP indicies.
(.:) :: IsGEP_Index vecSize opd idx a
  => a
  -> GEP_Indices vecSize opd idxs
  -> GEP_Indices vecSize opd (idx : idxs)
(.:) = toGEP_Indices
infixr 5 .:

-- | Helper function to convert GEP indicies to a list of untagged operands.
gepIndicesToOperands
  :: forall vecSize opd idxs. (Known vecSize, OperandFromKnownIndex opd)
  => GEP_Indices vecSize opd idxs
  -> [opd]

gepIndicesToOperands (GEP_KnownIndex tail) =
  let idxOpd :: forall i idxs'. (Known i, idxs ~ (Just i : idxs')) => opd
      idxOpd = operandFromKnownIndex (val @vecSize) (val @i)
  in idxOpd : gepIndicesToOperands tail
gepIndicesToOperands (GEP_UnknownIndex opd tail) = unTyped opd : gepIndicesToOperands tail
gepIndicesToOperands (GEP_UnknownIndexVector opd tail) = unTyped opd : gepIndicesToOperands tail
gepIndicesToOperands GEP_Nil = []

class OperandFromKnownIndex opd where
  operandFromKnownIndex :: Maybe Integer -> Integer -> opd

instance OperandFromKnownIndex Operand where
  operandFromKnownIndex vecSize i = ConstantOperand (operandFromKnownIndex vecSize i)

instance OperandFromKnownIndex Constant where
  operandFromKnownIndex Nothing  = Constant.Int 32
  operandFromKnownIndex (Just n) = Constant.Vector . take (fromIntegral n) . repeat . Constant.Int 32
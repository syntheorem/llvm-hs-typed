{-# LANGUAGE UndecidableInstances #-}

-- | This module provides a type-safe variant of "LLVM.AST.Constant".
module LLVM.AST.Tagged.Constant where

import Data.Word
import GHC.Exts (Constraint)

import LLVM.AST.TypeLevel
import LLVM.AST.Tagged.Type
import LLVM.AST.Tagged.Tag
import LLVM.AST.Constant
import LLVM.AST.Name (Name)
import LLVM.AST.Float (SomeFloat)
import LLVM.AST.IntegerPredicate (IntegerPredicate)
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate)

import Data.Coerce

-- A list of arguments to @getElementPtr@ which, on the type level,
-- tracks which arguments are statically known.
data GEP_Args (static_args :: [Maybe Nat])  where
    None     :: GEP_Args '[]
    -- | Statically known index. Only this is allowed to index into a structure
    AKnown   :: forall n xs. Known n =>
        GEP_Args xs ->
        GEP_Args (Just n : xs)
    -- | Dynamically known index.
    AUnknown :: forall width xs.
        Constant ::: IntegerType width ->
        GEP_Args xs ->
        GEP_Args (Nothing : xs)

-- | This type family calculates the return type of a 'getElementPtr' instruction.
type family GEP_Res (t :: Type) (as :: [Maybe Nat]) :: Type where
    GEP_Res t '[] = t
    GEP_Res (StructureType _ ts) (Just n : as) = GEP_Res (Nth ts n) as
    GEP_Res (ArrayType _ t2)     (_ : as) = GEP_Res t2 as


getGEPArgs :: forall static_args. GEP_Args static_args -> [Constant]
getGEPArgs None = []
getGEPArgs (AKnown as) =
    let i :: forall n xs . (Just n : xs) ~ static_args => Integer
            -- this extracts the n from the static args
        i = val @n
    in Int (word32Val @32) i : getGEPArgs as
getGEPArgs (AUnknown v as) = unTyped v : getGEPArgs as

getElementPtr :: forall (t :: Type) as static_args. Known t =>
    Bool ->
    Constant ::: PointerType as ->
    GEP_Args static_args ->
    Constant ::: PointerType as
getElementPtr in_bounds address indices
    = assertLLVMType $ GetElementPtr in_bounds (val @t) (unTyped address) (getGEPArgs indices)


int :: forall width. Known width => Integer -> Constant ::: IntegerType width
int value = assertLLVMType $ Int (word32Val @width) value

float :: forall fpt. SomeFloat :::: fpt -> Constant ::: FloatingPointType fpt
float = coerce Float

null :: forall as. Known as => Constant ::: PointerType as
null = coerce Null (val @(PointerType as))

struct :: forall b ts. Known b =>
    Maybe Name -> Constant :::* ts -> Constant ::: (StructureType b ts)
struct mbName xs = coerce Struct mbName (val @b) xs

array :: forall n t. Known t =>
    n × (Constant ::: t) -> Constant ::: (ArrayType n t)
array vals = coerce Array (val @t) (unCounted vals)

vector :: forall n t. Known t =>
    n × (Constant ::: t) -> Constant ::: (VectorType n t)
vector vals = coerce Vector (unCounted vals)

undef :: forall t. Known t => Constant ::: t
undef = coerce Undef (val @t)

-- TODO: Does it make sense to include BlockAddress here?

globalReference :: Name ::: t -> Constant ::: t
globalReference name = coerce GlobalReference name

tokenNone :: Constant ::: TokenType
tokenNone = coerce TokenNone

add :: forall width.
    Bool -> Bool ->
    Constant ::: IntegerType width -> Constant ::: IntegerType width ->
    Constant ::: IntegerType width
add = coerce Add

fadd :: forall fpt.
    Constant ::: (FloatingPointType fpt) -> Constant ::: (FloatingPointType fpt) ->
    Constant ::: (FloatingPointType fpt)
fadd = coerce FAdd

sub :: forall width.
    Bool -> Bool ->
    Constant ::: IntegerType width -> Constant ::: IntegerType width ->
    Constant ::: IntegerType width
sub = coerce Sub

fsub :: forall fpt.
    Constant ::: (FloatingPointType fpt) -> Constant ::: (FloatingPointType fpt) ->
    Constant ::: (FloatingPointType fpt)
fsub = coerce FSub

mul :: forall width.
    Bool -> Bool ->
    Constant ::: IntegerType width -> Constant ::: IntegerType width ->
    Constant ::: IntegerType width
mul = coerce Mul

fmul :: forall fpt.
    Constant ::: (FloatingPointType fpt) -> Constant ::: (FloatingPointType fpt) ->
    Constant ::: (FloatingPointType fpt)
fmul = coerce FMul

fdiv :: forall fpt.
    Constant ::: (FloatingPointType fpt) -> Constant ::: (FloatingPointType fpt) ->
    Constant ::: (FloatingPointType fpt)
fdiv = coerce FDiv

frem :: forall fpt.
    Constant ::: (FloatingPointType fpt) -> Constant ::: (FloatingPointType fpt) ->
    Constant ::: (FloatingPointType fpt)
frem = coerce FDiv

shl :: forall width.
    Bool -> Bool ->
    Constant ::: IntegerType width -> Constant ::: IntegerType width ->
    Constant ::: IntegerType width
shl = coerce Shl

lshr :: forall width.
    Bool ->
    Constant ::: IntegerType width -> Constant ::: IntegerType width ->
    Constant ::: IntegerType width
lshr = coerce LShr

ashr :: forall width.
    Bool ->
    Constant ::: IntegerType width -> Constant ::: IntegerType width ->
    Constant ::: IntegerType width
ashr = coerce AShr

and :: forall width.
    Constant ::: IntegerType width -> Constant ::: IntegerType width ->
    Constant ::: IntegerType width
and = coerce And

or :: forall width.
    Constant ::: IntegerType width -> Constant ::: IntegerType width ->
    Constant ::: IntegerType width
or = coerce Or

xor :: forall width.
    Constant ::: IntegerType width -> Constant ::: IntegerType width ->
    Constant ::: IntegerType width
xor = coerce Xor

trunc :: forall width1 width2. (Known width2, width2 <= width1) =>
    Constant ::: IntegerType width1 -> Constant ::: IntegerType width2
trunc o1 = coerce Trunc o1 (val @(IntegerType width2))

zext :: forall width1 width2. (Known width2, width1 <= width2) =>
    Constant ::: IntegerType width1 -> Constant ::: IntegerType width2
zext o1 = coerce ZExt o1 (val @(IntegerType width2))

sext :: forall width1 width2. (Known width2, width1 <= width2) =>
    Constant ::: IntegerType width1 -> Constant ::: IntegerType width2
sext o1 = coerce SExt o1 (val @(IntegerType width2))

fptoui :: forall fpt width. Known width =>
    Constant ::: FloatingPointType fpt ->
    Constant ::: IntegerType width
fptoui o1 = coerce FPToUI o1 (val @(IntegerType width))

fptosi :: forall fpt width. Known width =>
    Constant ::: FloatingPointType fpt ->
    Constant ::: IntegerType width
fptosi o1 = coerce FPToSI o1 (val @(IntegerType width))

uitofp :: forall width fpt. Known fpt =>
    Constant ::: IntegerType width ->
    Constant ::: FloatingPointType fpt
uitofp o1 = coerce UIToFP o1 (val @(FloatingPointType fpt))

sitofp :: forall width fpt. Known fpt =>
    Constant ::: IntegerType width ->
    Constant ::: FloatingPointType fpt
sitofp o1 = coerce SIToFP o1 (val @(FloatingPointType fpt))

fptrunc :: forall fpt1 fpt2.
    (Known fpt2, BitSizeOfFP fpt2 <= BitSizeOfFP fpt1) =>
    Constant ::: FloatingPointType fpt1 ->
    Constant ::: FloatingPointType fpt2
fptrunc o1 = coerce FPTrunc o1 (val @(FloatingPointType fpt2))

fpext :: forall fpt1 fpt2. Known fpt2 =>
    (Known fpt2, BitSizeOfFP fpt1 <= BitSizeOfFP fpt2) =>
    Constant ::: FloatingPointType fpt1 ->
    Constant ::: FloatingPointType fpt2
fpext o1 = coerce FPExt o1 (val @(FloatingPointType fpt2))

ptrtoint :: forall as width. Known width =>
    Constant ::: PointerType as ->
    Constant ::: IntegerType width
ptrtoint o1 = coerce PtrToInt o1 (val @(IntegerType width))

inttoptr :: forall as width. Known as =>
    Constant ::: IntegerType width ->
    Constant ::: PointerType as
inttoptr o1 = coerce IntToPtr o1 (val @(PointerType as))

bitcast :: forall t1 t2.
    (Known t2, NonAggregate t1, NonAggregate t2, BitSizeOf t1 ~ BitSizeOf t2) =>
    Constant ::: t1 -> Constant ::: t2
bitcast o1 = coerce BitCast o1 (val @t2)

icmp :: forall width.
    IntegerPredicate ->
    Constant ::: IntegerType width -> Constant ::: IntegerType width ->
    Constant ::: IntegerType width
icmp = coerce ICmp

fcmp :: forall width.
    FloatingPointPredicate ->
    Constant ::: IntegerType width -> Constant ::: IntegerType width ->
    Constant ::: IntegerType width
fcmp = coerce FCmp

select :: forall t.
    Constant ::: IntegerType 1 ->
    Constant ::: t -> Constant ::: t ->
    Constant ::: t
select = coerce Select

extractElement :: forall n t width.
    Constant ::: VectorType n t ->
    Constant ::: IntegerType width ->
    Constant ::: t
extractElement = coerce ExtractElement

insertElement :: forall n t width.
    Constant ::: VectorType n t ->
    Constant ::: t ->
    Constant ::: IntegerType width ->
    Constant ::: VectorType n t
insertElement = coerce InsertElement

shuffleVector :: forall n m t.
    Constant ::: VectorType n t ->
    Constant ::: VectorType n t ->
    Constant ::: VectorType m (IntegerType 32) ->
    Constant ::: VectorType m t
shuffleVector = coerce ShuffleVector

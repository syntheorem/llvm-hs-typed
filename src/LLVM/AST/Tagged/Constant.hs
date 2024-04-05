{-# LANGUAGE UndecidableInstances #-}

-- | This module provides a type-safe variant of "LLVM.AST.Constant".
module LLVM.AST.Tagged.Constant
( Constant

-- * Constant values
, int
, float
, struct
, array
, LLVM.AST.Tagged.Constant.vector
, LLVM.AST.Tagged.Constant.null
, zeroinitialize
, undef
, blockaddress
, global

-- * Constant expressions
, add
, fadd
, sub
, fsub
, mul
, fmul
, fdiv
, urem
, srem
, frem
, shl
, lshr
, ashr
, LLVM.AST.Tagged.Constant.and
, LLVM.AST.Tagged.Constant.or
, xor
, getelementptr
, trunc
, zext
, sext
, fptoui
, fptosi
, uitofp
, sitofp
, fptrunc
, fpext
, ptrtoint
, inttoptr
, bitcast
, addrspacecast
, icmp
, fcmp
, select
, extractelement
, insertelement
, shufflevector

-- * Additional utilities
, sizeof
) where

import Data.Word
import GHC.Exts (Constraint)

import LLVM.AST.TypeLevel
import LLVM.AST.Tagged.Type
import LLVM.AST.Tagged.Tag
import LLVM.AST.Tagged.GetElementPtr
import LLVM.AST.Tagged.AddrSpace
import LLVM.AST.Constant hiding (sizeof)
import LLVM.AST.Constant qualified as NonTagged
import LLVM.AST.Name (Name)
import LLVM.AST.Float (SomeFloat)
import LLVM.AST.IntegerPredicate (IntegerPredicate)
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate)

import Data.Coerce

int
  :: forall w.
     (ValidType (IntegerType w), Known w)
  => Integer
  -> Constant ::: IntegerType w
int value = assertLLVMType $ Int (word32Val @w) value

float
  :: ValidType (FloatingPointType fpt)
  => SomeFloat :::: fpt
  -> Constant ::: FloatingPointType fpt
float = coerce Float

struct
  :: forall packed ts.
     (ValidType (StructureType packed ts),
      Known packed)
  => Constant :::* ts
  -> Constant ::: (StructureType packed ts)
struct xs = assertLLVMType $ Struct Nothing (val @packed) (unTypedList xs)

array
  :: forall n t.
     (ValidType (ArrayType n t), Known t)
  => n × (Constant ::: t)
  -> Constant ::: (ArrayType n t)
array vals = coerce Array (val @t) (unCounted vals)

vector
  :: ValidType (VectorType n t)
  => n × (Constant ::: t)
  -> Constant ::: (VectorType n t)
vector vals = coerce Vector (unCounted vals)

null
  :: forall as.
     (ValidType (PointerType as), Known as)
  => Constant ::: PointerType as
null = coerce Null (val @(PointerType as))

zeroinitialize
  :: forall t.
     (ZeroInitializable t, Known t)
  => Constant ::: t
zeroinitialize = assertLLVMType $ AggregateZero (val @t)

-- TODO: SizedType might be too restrictive, should it be FirstClassType?
undef
  :: forall t.
     (SizedType t, Known t)
  => Constant ::: t
undef = coerce Undef (val @t)

blockaddress
  :: ValidType (PointerType as)
  => Name ::: PointerType as -- ^ function name
  -> Name ::: LabelType      -- ^ block label
  -> Constant ::: PointerType as
blockaddress = coerce BlockAddress

-- | Reference to the named global variable.
global :: ValidType t => Name ::: t -> Constant ::: t
global = coerce GlobalReference

tokenNone :: Constant ::: TokenType
tokenNone = coerce TokenNone

add
  :: IntOrIntVector n w t
  => Bool -- ^ nsw
  -> Bool -- ^ nuw
  -> Constant ::: t
  -> Constant ::: t
  -> Constant ::: t
add = coerce Add

fadd
  :: FloatOrFloatVector n fpt t
  => Constant ::: t
  -> Constant ::: t
  -> Constant ::: t
fadd = coerce FAdd

sub
  :: IntOrIntVector n w t
  => Bool -- ^ nsw
  -> Bool -- ^ nuw
  -> Constant ::: t
  -> Constant ::: t
  -> Constant ::: t
sub = coerce Sub

fsub
  :: FloatOrFloatVector n fpt t
  => Constant ::: t
  -> Constant ::: t
  -> Constant ::: t
fsub = coerce FSub

mul
  :: IntOrIntVector n w t
  => Bool -- ^ nsw
  -> Bool -- ^ nuw
  -> Constant ::: t
  -> Constant ::: t
  -> Constant ::: t
mul = coerce Mul

fmul
  :: FloatOrFloatVector n fpt t
  => Constant ::: t
  -> Constant ::: t
  -> Constant ::: t
fmul = coerce FMul

fdiv
  :: FloatOrFloatVector n fpt t
  => Constant ::: t
  -> Constant ::: t
  -> Constant ::: t
fdiv = coerce FDiv

urem
  :: IntOrIntVector n fpt t
  => Constant ::: t
  -> Constant ::: t
  -> Constant ::: t
urem = coerce URem

srem
  :: IntOrIntVector n fpt t
  => Constant ::: t
  -> Constant ::: t
  -> Constant ::: t
srem = coerce SRem

frem
  :: FloatOrFloatVector n fpt t
  => Constant ::: t
  -> Constant ::: t
  -> Constant ::: t
frem = coerce FRem

shl
  :: IntOrIntVector n w t
  => Bool -- ^ nsw
  -> Bool -- ^ nuw
  -> Constant ::: t
  -> Constant ::: t
  -> Constant ::: t
shl = coerce Shl

lshr
  :: IntOrIntVector n w t
  => Bool -- ^ exact
  -> Constant ::: t
  -> Constant ::: t
  -> Constant ::: t
lshr = coerce LShr

ashr
  :: IntOrIntVector n w t
  => Bool -- ^ exact
  -> Constant ::: t
  -> Constant ::: t
  -> Constant ::: t
ashr = coerce AShr

and
  :: IntOrIntVector n w t
  => Constant ::: t
  -> Constant ::: t
  -> Constant ::: t
and = coerce And

or
  :: IntOrIntVector n w t
  => Constant ::: t
  -> Constant ::: t
  -> Constant ::: t
or = coerce Or

xor
  :: IntOrIntVector n w t
  => Constant ::: t
  -> Constant ::: t
  -> Constant ::: t
xor = coerce Xor

getelementptr
  :: forall t pt as n idxs.
     (SizedType t,
      PtrOrPtrVector n as pt,
      GEP_IndicesValid t idxs,
      Known t, Known n)
  => Bool -- ^ inbounds
  -> Constant ::: pt -- ^ base address
  -> GEP_Indices n Constant idxs
  -> Constant ::: pt
getelementptr inBounds address indices = assertLLVMType $
  GetElementPtr inBounds (val @t) (coerce address) (gepIndicesToOperands indices)

trunc
  :: forall tyOut tyIn wOut wIn n.
     (IntOrIntVector n wOut tyOut,
      IntOrIntVector n wIn tyIn,
      wOut < wIn,
      Known tyOut)
  => Constant ::: tyIn
  -> Constant ::: tyOut
trunc o1 = coerce Trunc o1 (val @tyOut)

zext
  :: forall tyOut tyIn wOut wIn n.
     (IntOrIntVector n wOut tyOut,
      IntOrIntVector n wIn tyIn,
      wIn < wOut,
      Known tyOut)
  => Constant ::: tyIn
  -> Constant ::: tyOut
zext o1 = coerce ZExt o1 (val @tyOut)

sext
  :: forall tyOut tyIn wOut wIn n.
     (IntOrIntVector n wOut tyOut,
      IntOrIntVector n wIn tyIn,
      wIn < wOut,
      Known tyOut)
  => Constant ::: tyIn
  -> Constant ::: tyOut
sext o1 = coerce SExt o1 (val @tyOut)

fptoui
  :: forall tyOut tyIn wOut fptIn n.
     (IntOrIntVector n wOut tyOut,
      FloatOrFloatVector n fptIn tyIn,
      Known tyOut)
  => Constant ::: tyIn
  -> Constant ::: tyOut
fptoui o1 = coerce FPToUI o1 (val @tyOut)

fptosi
  :: forall tyOut tyIn wOut fptIn n.
     (IntOrIntVector n wOut tyOut,
      FloatOrFloatVector n fptIn tyIn,
      Known tyOut)
  => Constant ::: tyIn
  -> Constant ::: tyOut
fptosi o1 = coerce FPToSI o1 (val @tyOut)

uitofp
  :: forall tyOut tyIn fptOut wIn n.
     (FloatOrFloatVector n fptOut tyOut,
      IntOrIntVector n wIn tyIn,
      Known tyOut)
  => Constant ::: tyIn
  -> Constant ::: tyOut
uitofp o1 = coerce UIToFP o1 (val @tyOut)

sitofp
  :: forall tyOut tyIn fptOut wIn n.
     (FloatOrFloatVector n fptOut tyOut,
      IntOrIntVector n wIn tyIn,
      Known tyOut)
  => Constant ::: tyIn
  -> Constant ::: tyOut
sitofp o1 = coerce SIToFP o1 (val @tyOut)

fptrunc
  :: forall tyOut tyIn fptOut fptIn.
     (tyOut ~ FloatingPointType fptOut,
      tyIn ~ FloatingPointType fptIn,
      BitSizeOfFP fptOut < BitSizeOfFP fptIn,
      Known tyOut)
  => Constant ::: tyIn
  -> Constant ::: tyOut
fptrunc o1 = coerce FPTrunc o1 (val @tyOut)

fpext
  :: forall tyOut tyIn fptOut fptIn.
     (tyOut ~ FloatingPointType fptOut,
      tyIn ~ FloatingPointType fptIn,
      BitSizeOfFP fptIn < BitSizeOfFP fptOut,
      Known tyOut)
  => Constant ::: tyIn
  -> Constant ::: tyOut
fpext o1 = coerce FPExt o1 (val @tyOut)

ptrtoint
  :: forall tyOut tyIn wOut asIn n.
     (IntOrIntVector n wOut tyOut,
      PtrOrPtrVector n asIn tyIn,
      Known tyOut)
  => Constant ::: tyIn
  -> Constant ::: tyOut
ptrtoint o1 = coerce PtrToInt o1 (val @tyOut)

inttoptr
  :: forall tyOut tyIn asOut wIn n.
     (PtrOrPtrVector n asOut tyOut,
      IntOrIntVector n wIn tyIn,
      Known tyOut)
  => Constant ::: tyIn
  -> Constant ::: tyOut
inttoptr o1 = coerce IntToPtr o1 (val @tyOut)

bitcast
  :: forall tyOut tyIn.
     (BitCastable tyOut,
      BitCastable tyIn,
      BitSizeOf tyIn ~ BitSizeOf tyOut,
      Known tyOut)
  => Constant ::: tyIn
  -> Constant ::: tyOut
bitcast o1 = coerce BitCast o1 (val @tyOut)

addrspacecast
  :: forall tyOut tyIn asOut asIn n.
     (PtrOrPtrVector n asOut tyOut,
      PtrOrPtrVector n asIn tyIn,
      AddrSpaceNotEqual asIn asOut,
      Known tyOut)
  => Constant ::: tyIn
  -> Constant ::: tyOut
addrspacecast o1 = coerce AddrSpaceCast o1 (val @tyOut)

icmp
  :: forall t w n.
     IntOrIntVector n w t
  => IntegerPredicate
  -> Constant ::: t
  -> Constant ::: t
  -> Constant ::: ReplaceVectorType t I1
icmp = coerce ICmp

fcmp
  :: forall t fpt n.
     FloatOrFloatVector n fpt t
  => FloatingPointPredicate
  -> Constant ::: t
  -> Constant ::: t
  -> Constant ::: ReplaceVectorType t I1
fcmp = coerce FCmp

select
  :: forall t predTy n.
     (IntOrIntVector n 1 predTy,
      SizedType t, -- can't select a token type, not sure about metadata or label
      n ~ VectorLength t)
  => Constant ::: predTy
  -> Constant ::: t
  -> Constant ::: t
  -> Constant ::: t
select = coerce Select

extractelement
  :: forall t n w.
     (ValidType (VectorType n t),
      ValidType (IntegerType w))
  => Constant ::: VectorType n t
  -> Constant ::: IntegerType w
  -> Constant ::: t
extractelement = coerce ExtractElement

insertelement
  :: forall t n w.
     (ValidType (VectorType n t),
      ValidType (IntegerType w))
  => Constant ::: VectorType n t
  -> Constant ::: t
  -> Constant ::: IntegerType w
  -> Constant ::: VectorType n t
insertelement = coerce InsertElement

shufflevector
  :: forall t n m.
     (ValidType (VectorType n t),
      ValidType (VectorType m t))
  => Constant ::: VectorType n t
  -> Constant ::: VectorType n t
  -> Constant ::: VectorType m I32
  -> Constant ::: VectorType m t
shufflevector = coerce ShuffleVector

-- | platform independant sizeof: a gep to the end of a nullptr and some bitcasting.
sizeof
  :: forall (t :: Type) w.
     (SizedType t,
      ValidType (IntegerType w),
      Known t, Known w)
  => Constant ::: IntegerType w
sizeof = assertLLVMType $ NonTagged.sizeof (word32Val @w) (val @t)
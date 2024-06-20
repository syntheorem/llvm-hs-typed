{-# LANGUAGE UndecidableInstances #-}

-- | This module provides a type-safe variant of "LLVM.AST.Instruction".
--
-- 'Instruction's are tagged with their result type, but 'Terminator's have two tags. Given
-- @Terminator :::? r :::? a@, @a@ is the result type of the terminator, and @t@ is the return type
-- of the function the terminator is in. This enables type-level enforcement of return types.
module LLVM.AST.Tagged.Instruction
( InstructionMetadata

-- * Terminators
, Terminator
, ret
, ret_
, br_cond
, br
, switch
, indirectbr
, invoke
, invoke_asm
, invoke_va
, resume
, unreachable
, cleanupret
, catchret
, catchswitch

-- * Instructions
, Instruction

-- ** Arithmetic
, fneg
, add
, fadd
, sub
, fsub
, mul
, fmul
, udiv
, sdiv
, fdiv
, urem
, srem
, frem

-- ** Bitwise
, shl
, lshr
, ashr
, LLVM.AST.Tagged.Instruction.and
, LLVM.AST.Tagged.Instruction.or
, xor

-- ** Memory operations
, alloca
, alloca1
, alloca_va_list
, load
, load_atomic
, store
, store_atomic
, fence
, cmpxchg
, atomicrmw
, getelementptr

-- ** Conversion operations
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

-- ** Vector operations
, extractelement
, insertelement
, shufflevector

-- ** Aggregate operations
, extractvalue
, insertvalue

-- ** Other operations
, icmp
, fcmp
, phi
, freeze
, select
, call
, call_asm
, call_va
, va_arg
, catchpad
, cleanuppad
, landingpad

-- * landingpad clauses
, LandingPadClause
, catch
, LLVM.AST.Tagged.Instruction.filter

-- * Named values
, Named
, name
, do'

-- * Utilities
, arg
) where

import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty)
import Data.Int (Int32)
import Data.Word (Word32)

import LLVM.AST.Tagged.Tag
import LLVM.AST.Tagged.Type
import LLVM.AST.Tagged.Name
import LLVM.AST.Tagged.AddrSpace
import LLVM.AST.Tagged.Constant (Constant)
import LLVM.AST.Tagged.GetElementPtr
import LLVM.AST.Tagged.RMWOperation (RMWOperation)
import LLVM.AST.TypeLevel
import LLVM.AST.Operand (Operand, CallableOperand)
import LLVM.AST.Instruction
import LLVM.AST.Type qualified as NonTagged


import LLVM.AST.Tagged.CallingConvention (CallingConvention)
import LLVM.AST.Tagged.ParameterAttribute (ParameterAttribute)
import LLVM.AST.FunctionAttribute (FunctionAttribute, GroupID)
import LLVM.AST.Tagged.IntegerPredicate (IntegerPredicate)
import LLVM.AST.Tagged.FloatingPointPredicate (FloatingPointPredicate)
import LLVM.AST.Tagged.InlineAssembly (InlineAssembly)

-- | We distuingish between returning from a @void@ function and otherwise by
-- two different smart constructors (instead of passing a @Maybe@ to @Ret@).
ret
  :: ReturnType t
  => Operand ::: t
  -> InstructionMetadata
  -> Terminator :::! t :::? VoidResult
ret o im = coerce Ret (Just o) im

ret_
  :: InstructionMetadata
  -> Terminator :::? VoidResult :::? VoidResult
ret_ im = coerce (Ret Nothing) im

br_cond
  :: Operand ::: IntegerType 1
  -> Name ::: LabelType
  -> Name ::: LabelType
  -> InstructionMetadata
  -> Terminator :::? r :::? VoidResult
br_cond o1 n1 n2 im = coerce CondBr o1 n1 n2 im

br
  :: Name ::: LabelType
  -> InstructionMetadata
  -> Terminator :::? r :::? VoidResult
br n im = coerce Br n im

switch
  :: FirstClassType (IntegerType w)
  => Operand ::: IntegerType w
  -> Name ::: LabelType
  -> [(Constant ::: IntegerType w, Name ::: LabelType)]
  -> InstructionMetadata
  -> Terminator :::? r :::? VoidResult
switch o n targets im = coerce Switch o n targets im

indirectbr
  :: FirstClassType (PointerType as)
  => Operand ::: PointerType as
  -> [(Name ::: LabelType)]
  -> InstructionMetadata
  -> Terminator :::? r :::? VoidResult
indirectbr o ns im = coerce IndirectBr o ns im

invoke
  :: forall ret args as r.
     (ValidFunctionType ret args,
      FirstClassType (PointerType as),
      Known ret)
  => CallingConvention
  -> [ParameterAttribute] -- ^ return attributes
  -> Operand ::: PointerType as
  -> (Operand, [ParameterAttribute]) :::* args
  -> [Either GroupID FunctionAttribute] -- ^ function attributes
  -> Name ::: LabelType -- ^ return destination
  -> Name ::: LabelType -- ^ exception destination
  -> InstructionMetadata
  -> Terminator :::? r :::? ret
invoke cc pas o params
  = coerce Invoke cc pas (val @ret) (Right (coerce o) :: CallableOperand) (unTypedList params)

invoke_asm
  :: forall ret args as r.
     (ValidFunctionType ret args,
      FirstClassType (PointerType as),
      Known ret)
  => CallingConvention
  -> [ParameterAttribute] -- ^ return attributes
  -> InlineAssembly :::: FunctionType ret args
  -> (Operand, [ParameterAttribute]) :::* args
  -> [Either GroupID FunctionAttribute] -- ^ function attributes
  -> Name ::: LabelType -- ^ return destination
  -> Name ::: LabelType -- ^ exception destination
  -> InstructionMetadata
  -> Terminator :::? r :::? ret
invoke_asm cc pas ia params
  = coerce Invoke cc pas (val @ret) (Left (coerce ia) :: CallableOperand) (unTypedList params)

-- | Invoke a varargs function.
-- Argument list is split between the nonvariable and variable arguments.
invoke_va
  :: forall ret args varargs as r.
     (ValidFunctionType ret args,
      AllSatisfy FirstClassType varargs,
      FirstClassType (PointerType as),
      Known ret, Known args)
  => CallingConvention
  -> [ParameterAttribute] -- ^ return attributes
  -> Operand ::: PointerType as -- ^ function pointer
  -> (Operand, [ParameterAttribute]) :::* args -- ^ nonvariable arguments
  -> (Operand, [ParameterAttribute]) :::* varargs -- ^ variable arguments
  -> [Either GroupID FunctionAttribute] -- ^ function attributes
  -> Name ::: LabelType -- ^ return destination
  -> Name ::: LabelType -- ^ exception destination
  -> InstructionMetadata
  -> Terminator :::? r :::? ret
invoke_va cc pas o args varargs = coerce Invoke cc pas ty co argList
  where
    ty = NonTagged.FunctionType (val @ret) (val @args) True
    co :: CallableOperand = Right (coerce o)
    argList = unTypedList args ++ unTypedList varargs

-- | It is not checked that the type of the operand matches the type of
-- landingpads in this function.
resume
  :: FirstClassType t
  => Operand ::: t
  -> InstructionMetadata
  -> Terminator :::? r :::? VoidResult
resume o im = coerce Resume o im

unreachable
  :: InstructionMetadata
  -> Terminator :::? r :::? VoidResult
unreachable im = coerce Unreachable im

cleanupret
  :: Operand ::: TokenType
  -> Maybe (Name ::: LabelType)
  -> InstructionMetadata
  -> Terminator :::? r :::? VoidResult
cleanupret o mbn im = coerce CleanupRet o mbn im

catchret
  :: Operand ::: TokenType
  -> Name ::: LabelType
  -> InstructionMetadata
  -> Terminator :::? r :::? VoidResult
catchret o n im = coerce CatchRet o n im

catchswitch
  :: Operand ::: TokenType
  -> NonEmpty (Name ::: LabelType)
  -> Maybe (Name ::: LabelType)
  -> InstructionMetadata
  -> Terminator :::? r :::! TokenType
catchswitch o ns n im = coerce CatchSwitch o ns n im

-- instructions --

fneg
  :: FloatOrFloatVector n fpt t
  => FastMathFlags
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
fneg = coerce FNeg

add
  :: IntOrIntVector n w t
  => Bool -- ^ nsw
  -> Bool -- ^ nuw
  -> Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
add = coerce Add

fadd
  :: FloatOrFloatVector n fpt t
  => FastMathFlags
  -> Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
fadd = coerce FAdd

sub
  :: IntOrIntVector n w t
  => Bool -- ^ nsw
  -> Bool -- ^ nuw
  -> Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
sub = coerce Sub

fsub
  :: FloatOrFloatVector n fpt t
  => FastMathFlags
  -> Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
fsub = coerce FSub

mul
  :: IntOrIntVector n w t
  => Bool -- ^ nsw
  -> Bool -- ^ nuw
  -> Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
mul = coerce Mul

fmul
  :: FloatOrFloatVector n fpt t
  => FastMathFlags
  -> Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
fmul = coerce FMul

udiv
  :: IntOrIntVector n fpt t
  => Bool -- ^ exact
  -> Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
udiv = coerce UDiv

sdiv
  :: IntOrIntVector n fpt t
  => Bool -- ^ exact
  -> Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
sdiv = coerce SDiv

fdiv
  :: FloatOrFloatVector n fpt t
  => FastMathFlags
  -> Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
fdiv = coerce FDiv

urem
  :: IntOrIntVector n fpt t
  => Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
urem = coerce URem

srem
  :: IntOrIntVector n fpt t
  => Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
srem = coerce SRem

frem
  :: FloatOrFloatVector n fpt t
  => FastMathFlags
  -> Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
frem = coerce FRem

shl
  :: IntOrIntVector n w t
  => Bool -- ^ nsw
  -> Bool -- ^ nuw
  -> Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
shl = coerce Shl

lshr
  :: IntOrIntVector n w t
  => Bool -- ^ exact
  -> Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
lshr = coerce LShr

ashr
  :: IntOrIntVector n w t
  => Bool -- ^ exact
  -> Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
ashr = coerce AShr

and
  :: IntOrIntVector n w t
  => Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
and = coerce And

or
  :: IntOrIntVector n w t
  => Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
or = coerce Or

xor
  :: IntOrIntVector n w t
  => Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
xor = coerce Xor

-- | Allocate values on the stack.
--
-- The address space of the returned pointer is target-dependent (specified by the datalayout
-- string). Since we can't determine that at the type-level, we allow any address space to be used.
--
-- However, llvm-hs doesn't support specifying an address space for this instruction, so it will
-- actually always use the default address space regardless of what address space it is tagged with
-- at the type-level. This means that using any address space other than the default alloca address
-- space will result in an incorrectly-typed program.
alloca
  :: forall t as w.
     (SizedType t, Known t,
      FirstClassType (IntegerType w),
      FirstClassType (PointerType as))
  => Operand ::: IntegerType w -- ^ number of elements
  -> Word32 -- ^ alignment
  -> InstructionMetadata
  -> Instruction :::! PointerType as
alloca o align im = assertLLVMType $ Alloca (val @t) (Just (coerce o)) align im

-- | 'alloca' a single element
alloca1
  :: forall t as.
     (SizedType t, Known t,
      FirstClassType (PointerType as))
  => Word32 -- ^ alignment
  -> InstructionMetadata
  -> Instruction :::! PointerType as
alloca1 align im = assertLLVMType $ Alloca (val @t) Nothing align im

-- | 'alloca' the builtin @%struct.va_list@ type for use with varargs.
alloca_va_list
  :: (FirstClassType (PointerType as))
  => InstructionMetadata
  -> Instruction :::! PointerType as
alloca_va_list im = assertLLVMType $ Alloca va_list_type Nothing 0 im
  where va_list_type = NonTagged.NamedTypeReference (Name "struct.va_list")

load
  :: forall t as.
     (SizedType t, Known t,
      FirstClassType (PointerType as))
  => Bool -- ^ volatile
  -> Operand ::: PointerType as
  -> Word32 -- ^ alignment
  -> InstructionMetadata
  -> Instruction :::! t
load v o = coerce Load v (val @t) o (Nothing :: Maybe Atomicity)

load_atomic
  :: forall t as.
     (AtomicType t, Known t,
      FirstClassType (PointerType as))
  => Bool -- ^ volatile
  -> Operand ::: PointerType as
  -> Atomicity
  -> Word32 -- ^ alignment
  -> InstructionMetadata
  -> Instruction :::! t
load_atomic v o a = coerce Load v (val @t) o (Just a)

store
  :: forall t as.
     (SizedType t,
      FirstClassType (PointerType as))
  => Bool -- ^ volatile
  -> Operand ::: PointerType as
  -> Operand ::: t
  -> Word32 -- ^ alignment
  -> InstructionMetadata
  -> Instruction :::! t
store v o1 o2 = coerce Store v o1 o2 (Nothing :: Maybe Atomicity)

store_atomic
  :: forall t as.
     (AtomicType t,
      FirstClassType (PointerType as))
  => Bool -- ^ volatile
  -> Operand ::: PointerType as
  -> Operand ::: t
  -> Atomicity
  -> Word32 -- ^ alignment
  -> InstructionMetadata
  -> Instruction :::! t
store_atomic v o1 o2 a = coerce Store v o1 o2 (Just a)

fence
  :: Atomicity
  -> InstructionMetadata
  -> Instruction :::? VoidResult
fence = coerce Fence

cmpxchg
  :: forall t as.
     (AtomicType t,
      FirstClassType (PointerType as))
  => Bool -- ^ volatile
  -> Operand ::: PointerType as
  -> Operand ::: t -- ^ expected value
  -> Operand ::: t -- ^ replacement value
  -> Word32 -- ^ alignment
  -> Atomicity -- ^ syncscope and memory ordering on success
  -> MemoryOrdering -- ^ memory ordering on failure
  -> InstructionMetadata
  -> Instruction :::! StructureType False [t, I1]
cmpxchg = coerce CmpXchg

atomicrmw
  :: forall t as.
     (AtomicType t,
      FirstClassType (PointerType as))
  => Bool -- ^ volatile
  -> RMWOperation ::: t
  -> Operand ::: PointerType as
  -> Operand ::: t
  -> Word32 -- ^ alignment
  -> Atomicity
  -> InstructionMetadata
  -> Instruction :::! t
atomicrmw = coerce AtomicRMW

getelementptr
  :: forall t pt as n idxs.
     (SizedType t,
      PtrOrPtrVector n as pt,
      GEP_IndicesValid t idxs,
      Known t, Known n)
  => Bool -- ^ inbounds
  -> Operand ::: pt -- ^ base address
  -> GEP_Indices n Operand idxs
  -> InstructionMetadata
  -> Instruction :::! pt
getelementptr inBounds address indices im = assertLLVMType $
  GetElementPtr inBounds (val @t) (coerce address) (gepIndicesToOperands indices) im

trunc
  :: forall tyOut tyIn wOut wIn n.
     (IntOrIntVector n wOut tyOut,
      IntOrIntVector n wIn tyIn,
      wOut < wIn,
      Known tyOut)
  => Operand ::: tyIn
  -> InstructionMetadata
  -> Instruction :::! tyOut
trunc o1 = coerce Trunc o1 (val @tyOut)

zext
  :: forall tyOut tyIn wOut wIn n.
     (IntOrIntVector n wOut tyOut,
      IntOrIntVector n wIn tyIn,
      wIn < wOut,
      Known tyOut)
  => Operand ::: tyIn
  -> InstructionMetadata
  -> Instruction :::! tyOut
zext o1 = coerce ZExt o1 (val @tyOut)

sext
  :: forall tyOut tyIn wOut wIn n.
     (IntOrIntVector n wOut tyOut,
      IntOrIntVector n wIn tyIn,
      wIn < wOut,
      Known tyOut)
  => Operand ::: tyIn
  -> InstructionMetadata
  -> Instruction :::! tyOut
sext o1 = coerce SExt o1 (val @tyOut)

fptoui
  :: forall tyOut tyIn wOut fptIn n.
     (IntOrIntVector n wOut tyOut,
      FloatOrFloatVector n fptIn tyIn,
      Known tyOut)
  => Operand ::: tyIn
  -> InstructionMetadata
  -> Instruction :::! tyOut
fptoui o1 = coerce FPToUI o1 (val @tyOut)

fptosi
  :: forall tyOut tyIn wOut fptIn n.
     (IntOrIntVector n wOut tyOut,
      FloatOrFloatVector n fptIn tyIn,
      Known tyOut)
  => Operand ::: tyIn
  -> InstructionMetadata
  -> Instruction :::! tyOut
fptosi o1 = coerce FPToSI o1 (val @tyOut)

uitofp
  :: forall tyOut tyIn fptOut wIn n.
     (FloatOrFloatVector n fptOut tyOut,
      IntOrIntVector n wIn tyIn,
      Known tyOut)
  => Operand ::: tyIn
  -> InstructionMetadata
  -> Instruction :::! tyOut
uitofp o1 = coerce UIToFP o1 (val @tyOut)

sitofp
  :: forall tyOut tyIn fptOut wIn n.
     (FloatOrFloatVector n fptOut tyOut,
      IntOrIntVector n wIn tyIn,
      Known tyOut)
  => Operand ::: tyIn
  -> InstructionMetadata
  -> Instruction :::! tyOut
sitofp o1 = coerce SIToFP o1 (val @tyOut)

fptrunc
  :: forall tyOut tyIn fptOut fptIn.
     (tyOut ~ FloatingPointType fptOut,
      tyIn ~ FloatingPointType fptIn,
      BitSizeOfFP fptOut < BitSizeOfFP fptIn,
      Known tyOut)
  => Operand ::: tyIn
  -> InstructionMetadata
  -> Instruction :::! tyOut
fptrunc o1 = coerce FPTrunc o1 (val @tyOut)

fpext
  :: forall tyOut tyIn fptOut fptIn.
     (tyOut ~ FloatingPointType fptOut,
      tyIn ~ FloatingPointType fptIn,
      BitSizeOfFP fptIn < BitSizeOfFP fptOut,
      Known tyOut)
  => Operand ::: tyIn
  -> InstructionMetadata
  -> Instruction :::! tyOut
fpext o1 = coerce FPExt o1 (val @tyOut)

ptrtoint
  :: forall tyOut tyIn wOut asIn n.
     (IntOrIntVector n wOut tyOut,
      PtrOrPtrVector n asIn tyIn,
      Known tyOut)
  => Operand ::: tyIn
  -> InstructionMetadata
  -> Instruction :::! tyOut
ptrtoint o1 = coerce PtrToInt o1 (val @tyOut)

inttoptr
  :: forall tyOut tyIn asOut wIn n.
     (PtrOrPtrVector n asOut tyOut,
      IntOrIntVector n wIn tyIn,
      Known tyOut)
  => Operand ::: tyIn
  -> InstructionMetadata
  -> Instruction :::! tyOut
inttoptr o1 = coerce IntToPtr o1 (val @tyOut)

bitcast
  :: forall tyOut tyIn.
     (BitCastable tyOut,
      BitCastable tyIn,
      BitSizeOf tyIn ~ BitSizeOf tyOut,
      Known tyOut)
  => Operand ::: tyIn
  -> InstructionMetadata
  -> Instruction :::! tyOut
bitcast o1 = coerce BitCast o1 (val @tyOut)

addrspacecast
  :: forall tyOut tyIn asOut asIn n.
     (PtrOrPtrVector n asOut tyOut,
      PtrOrPtrVector n asIn tyIn,
      AddrSpaceNotEqual asIn asOut,
      Known tyOut)
  => Operand ::: tyIn
  -> InstructionMetadata
  -> Instruction :::! tyOut
addrspacecast o1 = coerce AddrSpaceCast o1 (val @tyOut)

extractelement
  :: forall t n w.
     (FirstClassType (VectorType n t),
      FirstClassType (IntegerType w))
  => Operand ::: VectorType n t
  -> Operand ::: IntegerType w
  -> InstructionMetadata
  -> Instruction :::! t
extractelement = coerce ExtractElement

insertelement
  :: forall t n w.
     (FirstClassType (VectorType n t),
      FirstClassType (IntegerType w))
  => Operand ::: VectorType n t
  -> Operand ::: t
  -> Operand ::: IntegerType w
  -> InstructionMetadata
  -> Instruction :::! VectorType n t
insertelement = coerce InsertElement

shufflevector
  :: forall t n m.
     (FirstClassType (VectorType n t),
      FirstClassType (VectorType m t))
  => Operand ::: VectorType n t
  -> Operand ::: VectorType n t
  -> m × Int32 -- TODO: have this be (Constant ::: VectorType m I32) instead
  -> InstructionMetadata
  -> Instruction :::! VectorType m t
shufflevector o1 o2 mask = coerce ShuffleVector o1 o2 (unCounted mask)

extractvalue
  :: forall (idxs :: [Nat]) t.
     (SizedType t, NotNull idxs, Known idxs)
  => Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! ValueAt t idxs
extractvalue o = coerce ExtractValue o (map fromIntegral (val @idxs) :: [Word32])

insertvalue
  :: forall (idxs :: [Nat]) t.
     (SizedType t, NotNull idxs, Known idxs)
  => Operand ::: t
  -> Operand ::: ValueAt t idxs
  -> InstructionMetadata
  -> Instruction :::! t
insertvalue o1 o2 = coerce InsertValue o1 o2 (map fromIntegral (val @idxs) :: [Word32])

icmp
  :: forall t w n.
     IntOrIntVector n w t
  => IntegerPredicate
  -> Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! ReplaceVectorType t I1
icmp = coerce ICmp

fcmp
  :: forall t fpt n.
     FloatOrFloatVector n fpt t
  => FloatingPointPredicate
  -> Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! ReplaceVectorType t I1
fcmp = coerce FCmp

-- can't select a token type, not sure about metadata or label
phi
  :: forall t.
     (SizedType t, Known t)
  => [(Operand ::: t, Name ::: LabelType)]
  -> InstructionMetadata
  -> Instruction :::! t
phi = coerce Phi (val @t)

-- TODO: SizedType might be too restrictive, should it be FirstClassType?
freeze
  :: forall t.
     (SizedType t, Known t)
  => Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
freeze o = coerce Freeze o (val @t)

-- can't select a token type, not sure about metadata or label
select
  :: forall t predTy n.
     (IntOrIntVector n 1 predTy,
      SizedType t,
      n ~ VectorLength t)
  => Operand ::: predTy
  -> Operand ::: t
  -> Operand ::: t
  -> InstructionMetadata
  -> Instruction :::! t
select = coerce Select

call
  :: forall ret args as.
     (ValidFunctionType ret args,
      FirstClassType (PointerType as),
      Known ret)
  => Maybe TailCallKind
  -> CallingConvention
  -> [ParameterAttribute] -- ^ return attributes
  -> Operand ::: PointerType as -- ^ function pointer
  -> (Operand, [ParameterAttribute]) :::* args
  -> [Either GroupID FunctionAttribute] -- ^ function attributes
  -> InstructionMetadata
  -> Instruction :::? ret
call tc cc pas o params
  = coerce Call tc cc pas (val @ret) (Right (coerce o) :: CallableOperand) (unTypedList params)

call_asm
  :: forall ret args as.
     (ValidFunctionType ret args,
      Known ret, Known args)
  => Maybe TailCallKind
  -> CallingConvention
  -> [ParameterAttribute] -- ^ return attributes
  -> InlineAssembly :::: FunctionType ret args
  -> (Operand, [ParameterAttribute]) :::* args
  -> [Either GroupID FunctionAttribute] -- ^ function attributes
  -> InstructionMetadata
  -> Instruction :::? ret
call_asm tc cc pas ia params
  = coerce Call tc cc pas (val @ret) (Left (coerce ia) :: CallableOperand) (unTypedList params)

-- | Call a varargs function.
-- Argument list is split between the nonvariable and variable arguments.
call_va
  :: forall ret args varargs as.
     (ValidFunctionType ret args,
      AllSatisfy FirstClassType varargs,
      FirstClassType (PointerType as),
      Known ret, Known args)
  => Maybe TailCallKind
  -> CallingConvention
  -> [ParameterAttribute] -- ^ return attributes
  -> Operand ::: PointerType as -- ^ function pointer
  -> (Operand, [ParameterAttribute]) :::* args -- ^ nonvariable arguments
  -> (Operand, [ParameterAttribute]) :::* varargs -- ^ variable arguments
  -> [Either GroupID FunctionAttribute] -- ^ function attributes
  -> InstructionMetadata
  -> Instruction :::? ret
call_va tc cc pas o args varargs = coerce Call tc cc pas ty co argList
  where
    ty = NonTagged.FunctionType (val @ret) (val @args) True
    co :: CallableOperand = Right (coerce o)
    argList = unTypedList args ++ unTypedList varargs

va_arg
  :: forall t as.
     (SizedType t, Known t,
      FirstClassType (PointerType as))
  => Operand ::: PointerType as -- ^ va_list pointer
  -> InstructionMetadata
  -> Instruction :::! t
va_arg o = coerce VAArg o (val @t)

catchpad
  :: AllSatisfy FirstClassType args
  => Operand ::: TokenType
  -> Operand :::* args -- ^ argument types depend on the target and exception handler personality
  -> InstructionMetadata
  -> Instruction :::! TokenType
catchpad o args = coerce CatchPad (unTyped o) (unTypedList args)

cleanuppad
  :: AllSatisfy FirstClassType args
  => Operand ::: TokenType
  -> Operand :::* args -- ^ argument types depend on the target and exception handler personality
  -> InstructionMetadata
  -> Instruction :::! TokenType
cleanuppad o args = coerce CleanupPad (unTyped o) (unTypedList args)

landingpad
  :: forall t clauses.
     (SizedType t, Known t,
      AllSatisfy SizedType clauses)
  => Bool -- ^ cleanup
  -> LandingPadClause :::* clauses
  -> InstructionMetadata
  -> Instruction :::! t
landingpad cleanup clauses = coerce LandingPad (val @t) cleanup (unTypedList clauses)

catch
  :: SizedType t
  => Constant ::: t
  -> LandingPadClause ::: t
catch = coerce Catch

filter
  :: FirstClassType (ArrayType n t)
  => Constant ::: ArrayType n t
  -> LandingPadClause ::: ArrayType n t
filter = coerce Filter

-- | This is the type-safe type corresponding to @Named Instruction@. It
-- enforces that an instruction has a name if and only if it is not a void
-- instruction, and that the name and instruction have the same type.
--
-- The returned 'Named Instruction' does not carry a type, because it is not
-- useful in any way.
name
  :: Name ::: t
  -> a :::! t
  -> Named a
name n a = unTyped n := unTyped a

-- | If you do have a void instruction, you must use 'do'' and not pass a name to it.
do' :: a :::? VoidResult -> Named a
do' a = Do (unTyped a)

-- | Helper function to construct an argument to a call or invoke instruction.
arg
  :: Operand ::: t
  -> [ParameterAttribute]
  -> (Operand, [ParameterAttribute]) ::: t
arg o pas = assertLLVMType (coerce o, pas)

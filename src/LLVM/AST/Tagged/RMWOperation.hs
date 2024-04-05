-- | This module provides a type-safe variant of "LLVM.AST.RMWOperation".
module LLVM.AST.Tagged.RMWOperation
( RMWOperation
, xchg
, add
, sub
, LLVM.AST.Tagged.RMWOperation.and
, nand
, LLVM.AST.Tagged.RMWOperation.or
, xor
, LLVM.AST.Tagged.RMWOperation.max
, LLVM.AST.Tagged.RMWOperation.min
, umax
, umin
, fadd
, fsub
) where

import LLVM.AST.RMWOperation

import LLVM.AST.Tagged.Tag
import LLVM.AST.Tagged.Type

xchg :: AtomicType t => RMWOperation ::: t
xchg = assertLLVMType Xchg

add :: AtomicType (IntegerType w) => RMWOperation ::: IntegerType w
add = assertLLVMType Add

sub :: AtomicType (IntegerType w) => RMWOperation ::: IntegerType w
sub = assertLLVMType Sub

and :: AtomicType (IntegerType w) => RMWOperation ::: IntegerType w
and = assertLLVMType And

nand :: AtomicType (IntegerType w) => RMWOperation ::: IntegerType w
nand = assertLLVMType Nand

or :: AtomicType (IntegerType w) => RMWOperation ::: IntegerType w
or = assertLLVMType Or

xor :: AtomicType (IntegerType w) => RMWOperation ::: IntegerType w
xor = assertLLVMType Xor

max :: AtomicType (IntegerType w) => RMWOperation ::: IntegerType w
max = assertLLVMType Max

min :: AtomicType (IntegerType w) => RMWOperation ::: IntegerType w
min = assertLLVMType Min

umax :: AtomicType (IntegerType w) => RMWOperation ::: IntegerType w
umax = assertLLVMType UMax

umin :: AtomicType (IntegerType w) => RMWOperation ::: IntegerType w
umin = assertLLVMType UMin

fadd :: AtomicType (FloatingPointType fpt) => RMWOperation ::: FloatingPointType fpt
fadd = assertLLVMType FAdd

fsub :: AtomicType (FloatingPointType fpt) => RMWOperation ::: FloatingPointType fpt
fsub = assertLLVMType FSub
-- | This module provides a type-safe variant of "LLVM.AST.InlineAssembly".
-- It is currently a stub
module LLVM.AST.Tagged.InlineAssembly
( InlineAssembly
, Dialect(..)
, inlineAssembly
) where

import LLVM.AST.InlineAssembly
import LLVM.AST.TypeLevel
import LLVM.AST.Tagged.Tag
import LLVM.AST.Tagged.Type

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)

inlineAssembly
  :: forall ret params.
     (ValidFunctionType ret params,
      Known ret, Known params)
  => ByteString -- ^ assembly
  -> ShortByteString -- ^ constraints
  -> Bool -- ^ has side effects
  -> Bool -- ^ align stack
  -> Dialect
  -> InlineAssembly :::: FunctionType ret params
inlineAssembly assembly constraints hasSideEffects alignStack dialect
  = assertLLVMType $ InlineAssembly
      { type' = val @(FunctionType ret params)
      , assembly, constraints, hasSideEffects, alignStack, dialect
      }
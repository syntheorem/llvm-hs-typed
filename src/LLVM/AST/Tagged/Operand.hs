-- | This module provides a type-safe variant of "LLVM.AST.Operand".
module LLVM.AST.Tagged.Operand
( Operand
, localReference
, constantOperand
, metadataOperand
, module LLVM.AST.Operand
) where

import LLVM.AST.Tagged.Constant (Constant)
import LLVM.AST.Tagged.Name (Name)
import LLVM.AST.Tagged.Tag
import LLVM.AST.Tagged.Type
import LLVM.AST.TypeLevel

import LLVM.AST.Operand hiding (Operand(..), CallableOperand)
import LLVM.AST.Operand (Operand)
import LLVM.AST.Operand qualified as NonTagged

localReference :: forall t. Known t => Name ::: t -> Operand ::: t
localReference nm = assertLLVMType (NonTagged.LocalReference (val @t) (unTyped nm))

constantOperand :: Constant ::: t -> Operand ::: t
constantOperand c = assertLLVMType (NonTagged.ConstantOperand (unTyped c))

metadataOperand :: Metadata -> Operand ::: MetadataType
metadataOperand md = assertLLVMType (NonTagged.MetadataOperand md)


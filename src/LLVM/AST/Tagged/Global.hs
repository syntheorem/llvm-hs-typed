{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides a type-safe variant of "LLVM.AST.Global".
module LLVM.AST.Tagged.Global
( Global
, UnnamedAddr(..)

-- * Global variables
, globalVariable
, GlobalVariableInfo
, globalVariableDefaults

-- * Global aliases
, globalAlias
, GlobalAliasInfo
, globalAliasDefaults

-- * Functions
, function
, FunctionInfo
, functionDefaults
, Parameter
, parameter
, BasicBlock
, basicBlock
) where

import Data.ByteString.Short (ShortByteString)
import Data.Coerce (coerce)
import Data.Word (Word32)

import LLVM.AST.AddrSpace qualified as NonTagged
import LLVM.AST.Type qualified as NonTagged
import LLVM.AST.Global qualified as NonTagged
import LLVM.AST.Global (Global(GlobalVariable, GlobalAlias, Function),
                        BasicBlock(..), Parameter(..), UnnamedAddr(..))
import LLVM.AST.Instruction (Named, Instruction, Terminator)

import LLVM.AST.Tagged.Tag
import LLVM.AST.Tagged.Type
import LLVM.AST.Tagged.Name
import LLVM.AST.Tagged.Instruction
import LLVM.AST.Tagged.AddrSpace
import LLVM.AST.Tagged.Constant (Constant)
import LLVM.AST.Tagged.Operand (MDNode, MDRef)
import LLVM.AST.Tagged.Linkage (Linkage)
import LLVM.AST.Tagged.Visibility (Visibility)
import LLVM.AST.Tagged.Attribute (ParameterAttribute, FunctionAttribute, GroupID)
import LLVM.AST.Tagged.CallingConvention (CallingConvention)
import LLVM.AST.Tagged.DLL qualified as DLL
import LLVM.AST.Tagged.ThreadLocalStorage qualified as TLS
import LLVM.AST.TypeLevel

-- | A global variable with an optional initializer.
globalVariable
  :: forall t as.
     (FirstClassType t,
      ValidType (PointerType as),
      Known t, Known as)
  => Name ::: PointerType as -- ^ variable name
  -> Maybe (Constant ::: t)  -- ^ initializer
  -> GlobalVariableInfo
  -> Global
globalVariable name initializer info
  = mkGlobalVariable (coerce name) (val @t) (val @as) (coerce initializer) info

mkGlobalVariable :: Name -> NonTagged.Type -> NonTagged.AddrSpace -> Maybe Constant -> GlobalVariableInfo -> Global
mkGlobalVariable name type' addrSpace initializer (GlobalVariableInfo {..}) = GlobalVariable {..}

data GlobalVariableInfo = GlobalVariableInfo
  { linkage :: Linkage
  , visibility :: Visibility
  , dllStorageClass :: Maybe DLL.StorageClass
  , threadLocalMode :: Maybe TLS.Model
  , unnamedAddr :: Maybe UnnamedAddr
  , isConstant :: Bool
  , section :: Maybe ShortByteString
  , comdat :: Maybe ShortByteString
  , alignment :: Word32
  , metadata :: [(ShortByteString, MDRef MDNode)]
  }

globalVariableDefaults :: GlobalVariableInfo
globalVariableDefaults = GlobalVariableInfo
  { linkage         = NonTagged.linkage         NonTagged.globalVariableDefaults
  , visibility      = NonTagged.visibility      NonTagged.globalVariableDefaults
  , dllStorageClass = NonTagged.dllStorageClass NonTagged.globalVariableDefaults
  , threadLocalMode = NonTagged.threadLocalMode NonTagged.globalVariableDefaults
  , unnamedAddr     = NonTagged.unnamedAddr     NonTagged.globalVariableDefaults
  , isConstant      = NonTagged.isConstant      NonTagged.globalVariableDefaults
  , section         = NonTagged.section         NonTagged.globalVariableDefaults
  , comdat          = NonTagged.comdat          NonTagged.globalVariableDefaults
  , alignment       = NonTagged.alignment       NonTagged.globalVariableDefaults
  , metadata        = NonTagged.metadata        NonTagged.globalVariableDefaults
  }

-- | A global alias definition.
--
-- The aliasee is an expression which computes a pointer to a global variable. @t@ is the type
-- pointed to by that pointer. (Note: I'm not 100% clear if that's what the type is supposed to be)
globalAlias
  :: forall (t :: Type) as.
     (ValidType t,
      ValidType (PointerType as),
      Known t, Known as)
  => Name ::: PointerType as     -- ^ alias name
  -> Constant ::: PointerType as -- ^ aliasee
  -> GlobalAliasInfo
  -> Global
globalAlias name aliasee info
  = mkGlobalAlias (coerce name) (val @t) (val @as) (coerce aliasee) info

mkGlobalAlias :: Name -> NonTagged.Type -> NonTagged.AddrSpace -> Constant -> GlobalAliasInfo -> Global
mkGlobalAlias name type' addrSpace aliasee (GlobalAliasInfo {..}) = GlobalAlias {..}

data GlobalAliasInfo = GlobalAliasInfo
  { linkage :: Linkage
  , visibility :: Visibility
  , dllStorageClass :: Maybe DLL.StorageClass
  , threadLocalMode :: Maybe TLS.Model
  , unnamedAddr :: Maybe UnnamedAddr
  }

globalAliasDefaults :: GlobalAliasInfo
globalAliasDefaults = GlobalAliasInfo
  { linkage         = NonTagged.linkage         NonTagged.globalAliasDefaults
  , visibility      = NonTagged.visibility      NonTagged.globalAliasDefaults
  , dllStorageClass = NonTagged.dllStorageClass NonTagged.globalAliasDefaults
  , threadLocalMode = NonTagged.threadLocalMode NonTagged.globalAliasDefaults
  , unnamedAddr     = NonTagged.unnamedAddr     NonTagged.globalAliasDefaults
  }

-- | A global function definition.
--
-- LLVM allows functions to be placed in different address spaces, but llvm-hs doesn't currently
-- support this, so the address space is hardcoded to 0.
function
  :: forall ret params.
     (ValidType (FunctionType ret params), Known ret)
  => Name ::: PointerType (AddrSpace 0) -- ^ function name
  -> Parameter :::* params
  -> Bool -- ^ variadic
  -> [BasicBlock ::: ret]
  -> FunctionInfo
  -> Global
function nm params variadic bbs info
  = mkFunction (coerce nm) (val @ret) (unTypedList params, variadic) (coerce bbs) info

mkFunction :: Name -> NonTagged.Type -> ([Parameter], Bool) -> [BasicBlock] -> FunctionInfo -> Global
mkFunction name returnType parameters basicBlocks (FunctionInfo {..}) = Function {..}

data FunctionInfo = FunctionInfo
  { linkage :: Linkage
  , visibility :: Visibility
  , dllStorageClass :: Maybe DLL.StorageClass
  , callingConvention :: CallingConvention
  , returnAttributes :: [ParameterAttribute]
  , functionAttributes :: [Either GroupID FunctionAttribute]
  , section :: Maybe ShortByteString
  , comdat :: Maybe ShortByteString
  , alignment :: Word32
  , garbageCollectorName :: Maybe ShortByteString
  , prefix :: Maybe Constant
  , personalityFunction :: Maybe Constant
  , metadata :: [(ShortByteString, MDRef MDNode)]
  }

functionDefaults :: FunctionInfo
functionDefaults = FunctionInfo
  { linkage              = NonTagged.linkage              NonTagged.functionDefaults
  , visibility           = NonTagged.visibility           NonTagged.functionDefaults
  , dllStorageClass      = NonTagged.dllStorageClass      NonTagged.functionDefaults
  , callingConvention    = NonTagged.callingConvention    NonTagged.functionDefaults
  , returnAttributes     = NonTagged.returnAttributes     NonTagged.functionDefaults
  , functionAttributes   = NonTagged.functionAttributes   NonTagged.functionDefaults
  , section              = NonTagged.section              NonTagged.functionDefaults
  , comdat               = NonTagged.comdat               NonTagged.functionDefaults
  , alignment            = NonTagged.alignment            NonTagged.functionDefaults
  , garbageCollectorName = NonTagged.garbageCollectorName NonTagged.functionDefaults
  , prefix               = NonTagged.prefix               NonTagged.functionDefaults
  , personalityFunction  = NonTagged.personalityFunction  NonTagged.functionDefaults
  , metadata             = NonTagged.metadata             NonTagged.functionDefaults
  }

parameter
  :: forall t. Known t
  => Name ::: t
  -> [ParameterAttribute]
  -> Parameter ::: t
parameter nm attrs = coerce Parameter (val @t) nm attrs

basicBlock
  :: Name ::: LabelType
  -> [Named Instruction]
  -> Named (Terminator ::: t)
  -> BasicBlock ::: t
basicBlock nm instr term = assertLLVMType $ BasicBlock (coerce nm) instr (coerce term)
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides a type-safe variant of "LLVM.IRBuilder" interface.
module LLVM.AST.Tagged.IRBuilder
( -- * Module builder
  ModuleBuilder
, ModuleBuilderT
, runModuleBuilder
, runModuleBuilderT
, execModuleBuilder
, execModuleBuilderT
, MonadModuleBuilder(..)
, ModuleBuilderState
, emptyModuleBuilder

-- ** Module definitions
, emitDefn
, ParameterInfo
, namedParam
, param
, defineFunction
, declareFunction
, global

-- * IR builder
, IRBuilder
, IRBuilderT
, MonadIRBuilder(..)
, IRBuilderState
, runIRBuilder
, runIRBuilderT
, execIRBuilder
, execIRBuilderT

-- ** Core functionality
, inst
, inst_
, term
, term_
, block
, named
) where

import LLVM.Prelude
import GHC.Stack (HasCallStack)

import Control.Monad.Cont (MonadCont, ContT)
import Control.Monad.Except (MonadError, ExceptT)
import Control.Monad.Identity (IdentityT, runIdentity, Identity)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.RWS.Lazy qualified as Lazy
import Control.Monad.RWS.Strict qualified as Strict
import Control.Monad.State.Strict
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Writer.Lazy qualified as Lazy
import Control.Monad.Writer.Strict qualified as Strict

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing, isJust)
import Data.String (fromString)

import LLVM.IRBuilder qualified as NonTagged
import LLVM.IRBuilder
  ( ModuleBuilderT
  , ModuleBuilder
  , MonadModuleBuilder(..)
  , ModuleBuilderState
  , emptyModuleBuilder
  , runModuleBuilder
  , runModuleBuilderT
  , execModuleBuilder
  , execModuleBuilderT
  , emitDefn
  )

import LLVM.AST.Tagged.AddrSpace
import LLVM.AST.Tagged.Constant (Constant)
import LLVM.AST.Tagged.Constant qualified as Constant
import LLVM.AST.Tagged.Global
import LLVM.AST.Tagged.Instruction (Instruction, Terminator, Named)
import LLVM.AST.Tagged.Instruction qualified as Inst
import LLVM.AST.Tagged.Name
import LLVM.AST.Tagged.Operand (Operand, localReference, constantOperand)
import LLVM.AST.Tagged.Tag
import LLVM.AST.Tagged.Type
import LLVM.AST.TypeLevel
import LLVM.AST.Tagged.ParameterAttribute (ParameterAttribute)
import LLVM.AST.Tagged (Definition(..))

newtype IRBuilderT (ret :: Type) m a = IRBuilderT (StateT (IRBuilderState ret) m a)
  deriving newtype
    ( Functor, Alternative, Applicative, Monad, MonadCont, MonadFix, MonadPlus
    , MonadError e, MonadIO, MonadReader r, MonadFail, MonadTrans, MonadWriter w
    , MonadModuleBuilder
    )

type IRBuilder ret = IRBuilderT ret Identity

class Monad m => MonadIRBuilder (ret :: Type) m | m -> ret where
  liftIRState :: State (IRBuilderState ret) a -> m a

  default liftIRState :: (MonadTrans t, MonadIRBuilder ret m', m ~ t m') => State (IRBuilderState ret) a -> m a
  liftIRState = lift . liftIRState

instance Monad m => MonadIRBuilder ret (IRBuilderT ret m) where
  liftIRState (StateT s) = IRBuilderT $ StateT $ pure . runIdentity . s

-- | Builder monad state
data IRBuilderState (ret :: Type) = IRBuilderState
  { builderSupply :: !Word
  , builderUsedNames :: !(Map ShortByteString Word)
  , builderNameSuggestion :: !(Maybe ShortByteString)
  , builderBlocks :: [BasicBlock ::: ret]
  , builderBlockName :: !(Maybe (Name ::: LabelType))
  , builderBlockInsts :: [Named Instruction]
  }

runIRBuilderT :: (HasCallStack, Monad m) => IRBuilderT ret m a -> m (a, [BasicBlock ::: ret])
runIRBuilderT builder = do
  (a, s) <- runStateT m initState
  pure (a, reverse (builderBlocks s))
  where
    IRBuilderT m = do
      a <- builder
      mName <- liftIRState $ gets builderBlockName
      if isJust mName
        then error "unterminated basic block"
        else pure a

    initState = IRBuilderState
      { builderSupply = 0
      , builderUsedNames = Map.empty
      , builderNameSuggestion = Nothing
      , builderBlocks = []
      , builderBlockName = Nothing
      , builderBlockInsts = []
      }

runIRBuilder :: HasCallStack => IRBuilder ret a -> (a, [BasicBlock ::: ret])
runIRBuilder = runIdentity . runIRBuilderT

execIRBuilderT :: (HasCallStack, Monad m) => IRBuilderT ret m a -> m [BasicBlock ::: ret]
execIRBuilderT = fmap snd . runIRBuilderT

execIRBuilder :: HasCallStack => IRBuilder ret a -> [BasicBlock ::: ret]
execIRBuilder = snd . runIRBuilder

-- | Generate a fresh name. The resulting name is numbered or
-- based on the name suggested with 'named' if that's used.
fresh :: MonadIRBuilder ret m => m (Name ::: t)
fresh = do
  msuggestion <- liftIRState $ gets builderNameSuggestion
  maybe freshUnName freshName msuggestion

-- | Generate a fresh name from a name suggestion
freshName :: MonadIRBuilder ret m => ShortByteString -> m (Name ::: t)
freshName suggestion = do
  -- TODO: validate name suggestion
  usedNames <- liftIRState $ gets builderUsedNames
  let
    nameCount = fromMaybe 0 $ Map.lookup suggestion usedNames
    unusedName = suggestion <> fromString ("_" <> show nameCount)
    updatedUsedNames = Map.insert suggestion (nameCount + 1) usedNames
  liftIRState $ modify $ \s -> s { builderUsedNames = updatedUsedNames }
  return $ assertLLVMType $ Name unusedName

-- | Generate a fresh numbered name
freshUnName :: MonadIRBuilder ret m => m (Name ::: t)
freshUnName = liftIRState $ do
  n <- gets builderSupply
  modify $ \s -> s { builderSupply = 1 + n }
  pure $ assertLLVMType $ UnName n

ensureBlock :: MonadIRBuilder ret m => m (Name ::: LabelType)
ensureBlock = do
  mName <- liftIRState $ gets builderBlockName
  case mName of
    Just name -> pure name
    Nothing -> do
      name <- freshUnName
      liftIRState $ modify \s -> s { builderBlockName = Just name }
      pure name

appendInst :: MonadIRBuilder ret m => Named Instruction -> m ()
appendInst ni = do
  ensureBlock
  liftIRState $ modify \s -> s { builderBlockInsts = ni : builderBlockInsts s }

terminateBlock :: MonadIRBuilder ret m => Named (Terminator ::: ret) -> m ()
terminateBlock nt = do
  blockName <- ensureBlock
  blockInsts <- liftIRState $ gets (reverse . builderBlockInsts)
  let block = basicBlock blockName blockInsts nt
  liftIRState $ modify \s -> s
    { builderBlocks = block : builderBlocks s
    , builderBlockName = Nothing
    , builderBlockInsts = []
    }

inst :: (MonadIRBuilder ret m, FirstClassType t, Known t) => Instruction ::: t -> m (Operand ::: t)
inst i = do
  name <- fresh
  appendInst $ Inst.name name i
  pure $ localReference name

inst_ :: MonadIRBuilder ret m => Instruction ::: VoidType -> m ()
inst_ i = appendInst $ Inst.do' i

term :: (MonadIRBuilder ret m, FirstClassType t, Known t) => Terminator ::: ret ::: t -> m (Operand ::: t)
term t = do
  name <- fresh
  terminateBlock $ Inst.name name t
  pure $ localReference name

term_ :: MonadIRBuilder ret m => Terminator ::: ret ::: VoidType -> m ()
term_ t = terminateBlock $ Inst.do' t

-- | Begin a new basic block.
--
-- If the previous basic block has not been terminated, then an unconditional branch to the newly
-- created block will be inserted.
block :: MonadIRBuilder ret m => m (Name ::: LabelType)
block = do
  currBlock <- liftIRState $ gets builderBlockName
  newBlock <- fresh

  when (isJust currBlock) do
    -- there is currently an active block, need to terminate it
    term_ $ Inst.br newBlock []

  liftIRState $ modify \s -> s { builderBlockName = Just newBlock }
  pure newBlock

-- | @ir `named` name@ executes the 'IRBuilder' @ir@ using @name@ as the base
-- name whenever a fresh local name is generated. Collisions are avoided by
-- appending numbers (first @"name"@, then @"name1"@, @"name2"@, and so on).
named
  :: MonadIRBuilder ret m
  => m r
  -> ShortByteString
  -> m r
named ir name = do
  before <- liftIRState $ gets builderNameSuggestion
  liftIRState $ modify $ \s -> s { builderNameSuggestion = Just name }
  result <- ir
  liftIRState $ modify $ \s -> s { builderNameSuggestion = before }
  pure result

-------------------------------------------------------------------------------
-- top-level declarations
-------------------------------------------------------------------------------

type ParameterInfo = (Maybe ShortByteString, [ParameterAttribute])

namedParam :: ShortByteString -> [ParameterAttribute] -> ParameterInfo ::: t
namedParam name attrs = assertLLVMType (Just name, attrs)

param :: [ParameterAttribute] -> ParameterInfo ::: t
param attrs = assertLLVMType (Nothing, attrs)

-- | Emit a function definition.
defineFunction
  :: forall ret params m.
     (MonadModuleBuilder m,
      ValidType (FunctionType ret params),
      Known ret, AllSatisfy Known params)
  => Name ::: PointerType (AddrSpace 0) -- ^ function name
  -> ParameterInfo :::* params
  -> Bool -- ^ variadic
  -> FunctionInfo
  -> (Operand :::* params -> IRBuilderT ret m ())
  -> m (Constant ::: PointerType (AddrSpace 0))
defineFunction name paramInfo variadic info body = do
  (params, blocks) <- runIRBuilderT do
    (params, operands) <- convertParamInfo paramInfo
    body operands
    pure params
  emitDefn (GlobalDefinition (function name params variadic blocks info))
  pure (Constant.global name)
  where
    convertParamInfo
      :: forall (ps :: [Type]).
         AllSatisfy Known ps
      => ParameterInfo :::* ps
      -> IRBuilderT ret m (Parameter :::* ps, Operand :::* ps)

    convertParamInfo TNil = pure (TNil, TNil)
    convertParamInfo (Typed (mName, attrs) :* rest) = do
      name <- case mName of
        Nothing -> freshUnName
        Just nm -> freshName nm
      (params, operands) <- convertParamInfo rest
      pure (parameter name attrs :* params, localReference name :* operands)

-- | Emit a function declaration.
declareFunction
  :: forall ret params m.
     (MonadModuleBuilder m,
      ValidType (FunctionType ret params),
      Known ret, AllSatisfy Known params)
  => Name ::: PointerType (AddrSpace 0) -- ^ function name
  -> ParameterInfo :::* params -- ^ parameter name is ignored
  -> Bool -- ^ variadic
  -> FunctionInfo
  -> m (Constant ::: PointerType (AddrSpace 0))
declareFunction name paramInfo variadic info = do
  let params = convertParamInfo paramInfo
  emitDefn (GlobalDefinition (function @ret name params variadic [] info))
  pure (Constant.global name)
  where
    convertParamInfo :: forall (ps :: [Type]). AllSatisfy Known ps => ParameterInfo :::* ps -> Parameter :::* ps
    convertParamInfo TNil = TNil
    convertParamInfo (Typed (_, attrs) :* rest)
      = parameter (assertLLVMType (mkName "")) attrs :* convertParamInfo rest

-- | Emit a global variable with an optional initializer.
global
  :: forall t as m.
     (MonadModuleBuilder m,
      FirstClassType t,
      ValidType (PointerType as),
      Known t, Known as)
  => Name ::: PointerType as -- ^ variable name
  -> Maybe (Constant ::: t)  -- ^ initializer
  -> GlobalVariableInfo
  -> m (Constant ::: PointerType as)
global name init info = do
  emitDefn (GlobalDefinition (globalVariable name init info))
  pure (Constant.global name)

-------------------------------------------------------------------------------
-- mtl instances
-------------------------------------------------------------------------------

instance MonadState s m => MonadState s (IRBuilderT ret m) where
  state = lift . state

instance MonadIRBuilder ret m => MonadIRBuilder ret (ContT r m)
instance MonadIRBuilder ret m => MonadIRBuilder ret (ExceptT e m)
instance MonadIRBuilder ret m => MonadIRBuilder ret (IdentityT m)
instance MonadIRBuilder ret m => MonadIRBuilder ret (ReaderT r m)
instance (MonadIRBuilder ret m, Monoid w) => MonadIRBuilder ret (Strict.RWST r w s m)
instance (MonadIRBuilder ret m, Monoid w) => MonadIRBuilder ret (Lazy.RWST r w s m)
instance MonadIRBuilder ret m => MonadIRBuilder ret (StateT s m)
instance MonadIRBuilder ret m => MonadIRBuilder ret (Lazy.StateT s m)
instance (Monoid w, MonadIRBuilder ret m) => MonadIRBuilder ret (Strict.WriterT w m)
instance (Monoid w, MonadIRBuilder ret m) => MonadIRBuilder ret (Lazy.WriterT w m)
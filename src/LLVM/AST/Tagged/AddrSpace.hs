-- | This module provides a type-safe variant of "LLVM.AST.AddrSpace".
-- It is currently a stub
module LLVM.AST.Tagged.AddrSpace where

import LLVM.AST.AddrSpace qualified as NonTagged
import LLVM.AST.TypeLevel

-- | Redefinition of 'AST.AddrSpace' for use at the type-level.
--
-- The type name has @'@ appended to it to distinguish it from the constructor name, since both will
-- be used at the type-level.
data AddrSpace' = AddrSpace Nat

type family AddrSpaceNotEqual (as1 :: AddrSpace') (as2 :: AddrSpace') :: Constraint where
  AddrSpaceNotEqual (AddrSpace n) (AddrSpace n) = TypeError (Text "address spaces are equal")
  AddrSpaceNotEqual _ _ = ()

type instance Value AddrSpace' = NonTagged.AddrSpace
instance Known n => Known (AddrSpace n) where knownVal = NonTagged.AddrSpace (word32Val @n)
module LLVM.AST.Tagged.Tag where

import Data.Coerce

import LLVM.AST.Type qualified as NonTagged
import LLVM.AST.TypeLevel
import LLVM.AST.Tagged.Type (Type)

-- | A value of type @v ::: t@ denotes a value of type @v@ with an LLVM type
-- annotation of @t :: Type@.
--
-- This anntation exists only on the Haskell type level. Functions that need to
-- get hold of the actual 'LLVM.Ast.Type.Type' associated to the tag will
-- typically have a 'Known' type class constraint.
type v ::: (t :: Type) = v :::: t

-- | Sometimes we want to annotate a value @v@ with something else than an LLVM
-- type (@Type@), so this allows any kind
newtype v :::: (t :: k) = Typed v


-- | Adds an LLVM type annotation to its argument. Note that this function is unchecked.
assertLLVMType :: v -> v :::: t
assertLLVMType = Typed

-- | Removes the LLVM type annotation.
unTyped :: v :::: t -> v
unTyped (Typed v) = v


-- | Removes the LLVM type annotation.
typeOf :: forall (t :: Type) v. Known t => (v ::: t) -> NonTagged.Type
typeOf (Typed v) = (val @t)

-- | A list of tagged values. The smart constructors below ensure
-- that the type-level list has the same lengths as the value list,
-- and that the elements have the corresponding tag.
type v :::* (ts :: [k']) = [v] :::: ts

tnil :: v :::* '[]
tnil = assertLLVMType []

pattern (:*) :: v :::: t -> v :::* ts -> v :::* (t:ts)
pattern x :* xs <- (unTyped -> ((coerce -> x) : (coerce -> xs) :: [v]))
  where
    (:*) x xs = assertLLVMType (unTyped x : unTyped xs)

infixr 5 :*

-- | A vector type
data (n::Nat) × a where
    VNil   ::               0 × a
    (:×)   :: a -> n × a -> (1 + n) × a
infixr 5 :×

unCounted :: n × a -> [a]
unCounted VNil = []
unCounted (x :× xs) = x : unCounted xs

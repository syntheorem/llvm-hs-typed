module LLVM.AST.Tagged.Tag where

import LLVM.AST.Type qualified as NonTagged
import LLVM.AST.TypeLevel
import LLVM.AST.Tagged.Type

-- | Sometimes we want to annotate a value @v@ with something else than an LLVM
-- type (@Type@), so this allows any kind
newtype v :::: (t :: k) = Typed v

-- | Adds an LLVM type annotation to its argument. Note that this function is unchecked.
assertLLVMType :: v -> v :::: t
assertLLVMType = Typed

-- | Removes the LLVM type annotation.
unTyped :: v :::: t -> v
unTyped (Typed v) = v

-- | A value of type @v ::: t@ denotes a value of type @v@ with an LLVM type
-- annotation of @t :: Type@.
--
-- This anntation exists only on the Haskell type level. Functions that need to
-- get hold of the actual 'LLVM.Ast.Type.Type' associated to the tag will
-- typically have a 'Known' type class constraint.
type v ::: (t :: Type) = v :::: t
infixl :::

-- | Get the value-level type of an annotated value.
typeOf :: forall t v. Known t => (v ::: t) -> NonTagged.Type
typeOf _ = (val @t)

type v :::? (r :: Result') = v :::: r
infixl :::?

type v :::! (t :: Type) = v :::: Result t
infixl :::!

-- | A list of tagged values.
data v :::* (ts :: [k]) where
  TNil :: v :::* '[]
  (:*) :: (v :::: t) -> (v :::* ts) -> v :::* (t:ts)
infixr 5 :*

unTypedList :: v :::* ts -> [v]
unTypedList TNil = []
unTypedList (v :* vs) = unTyped v : unTypedList vs

-- | A vector type
data (n::Nat) × a where
  VNil :: 0 × a
  (:×) :: a -> n × a -> (1 + n) × a
infixr 5 :×

unCounted :: n × a -> [a]
unCounted VNil = []
unCounted (x :× xs) = x : unCounted xs

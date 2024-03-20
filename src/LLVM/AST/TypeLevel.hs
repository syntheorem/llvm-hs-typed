{-# LANGUAGE UndecidableInstances #-}

-- | This module also contains various type-level functions, classes and other machinery that
-- provide the necessary functionality for the typed AST.
module LLVM.AST.TypeLevel
( Value
, Known(knownVal)
, val
, wordVal
, word32Val
, word64Val
, byteStringVal

-- * Re-exports
, Nat
, Symbol
, TypeError
, ErrorMessage(..)
, type (-)
, type (+)
, type (*)
, type (<=)

-- * Other utilities
, Nth
, NotNull
) where

import Data.ByteString.Short qualified as BS
import Data.String.Encode (convertString)
import Data.Word (Word, Word32, Word64)
import GHC.Exts (Constraint)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal, Nat, KnownNat, natVal,
                    TypeError, ErrorMessage(..), type (-), type (+), type (*), type (<=))

-- | This type family indicates the value-level representation of a type-level
-- type. Often these are the same.
type family Value k

-- | This class connects type variables (of kind @k@) to their value-level
-- representation (of type 'Value k').
class Known (t :: k)  where
    knownVal :: Value k

-- | Helper function equivalent to 'knownVal' but with the kind inferred, so it doesn't have to be
-- supplied as a type application.
val :: forall {k} (t :: k). Known t => Value k
val = knownVal @k @t

wordVal :: forall (n::Nat). Known n => Word
wordVal = fromIntegral (val @n)

word32Val :: forall (n::Nat). Known n => Word32
word32Val = fromIntegral (val @n)

word64Val :: forall (n::Nat). Known n => Word64
word64Val = fromIntegral (val @n)

byteStringVal :: forall (s::Symbol). Known s => BS.ShortByteString
byteStringVal = convertString (val @s)

type instance Value [a] = [Value a]
instance Known '[] where knownVal = []
instance (Known t, Known ts) => Known (t:ts) where knownVal = val @t : val @ts

type instance Value (a, b) = (Value a, Value b)
instance (Known a, Known b) => Known '(a, b) where knownVal = (val @a, val @b)

type instance Value Bool = Bool
instance Known True where knownVal = True
instance Known False where knownVal = False

type instance Value Symbol = String
instance KnownSymbol s => Known s where knownVal = symbolVal @s undefined

type instance Value Nat = Integer
instance KnownNat n => Known n where knownVal = natVal @n undefined

-- | Get the nth element of a type-level list.
type family Nth (xs :: [a]) n :: a where
    Nth '[] 0 = TypeError (Text "empty list")
    Nth (x:xs) 0 = x
    Nth (x:xs) n = Nth xs (n - 1)

-- | Constraint requiring a type-level list to be non-empty.
type family NotNull (xs :: [a]) :: Constraint  where
    NotNull '[] = TypeError (Text "The list must not be empty")
    NotNull _ = ()
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveGeneric         #-}

module Numeric.Sized.Word (Word(..)) where

import           Data.Bits
import           Data.Coerce
import           Data.Function
import           Data.Proxy
import           GHC.TypeLits
import           Numeric.Natural
import           Prelude                hiding (Word)
import           Test.QuickCheck        (Arbitrary (..), arbitraryBoundedEnum)
import           Test.SmallCheck.Series

-- | A very small numeric type for exhaustiveness, with wraparound behavior
newtype Word (n :: Nat) = Word { getWord :: Natural } deriving Generic

instance KnownNat n => Bounded (Word n) where
  minBound = Word 0
  maxBound = Word (shift 1 (fromInteger (natVal (Proxy :: Proxy n))) - 1)

type CoerceBinary a b = (a -> a -> a) -> (b -> b -> b)

instance KnownNat n => Bits (Word n) where
 (.&.)        = (coerce :: CoerceBinary Natural (Word n)) (.&.)
 (.|.)        = (coerce :: CoerceBinary Natural (Word n)) (.|.)
 xor          = trunc .: (coerce :: CoerceBinary Natural (Word n)) xor
 complement   = trunc .  (coerce :: (Natural -> Natural)
                                 ->  Word n  -> Word n
                         )  complement
 shift        = trunc .: (coerce :: (Natural -> Prelude.Int -> Natural)
                                 ->  Word n  -> Prelude.Int -> Word n
                         ) shift
 rotate       = trunc .: (coerce :: (Natural -> Prelude.Int -> Natural)
                                 -> Word n   -> Prelude.Int -> Word n
                         ) rotate
 bit          = trunc . Word . bit
 bitSize      = fromInteger . natVal
 bitSizeMaybe = Just . fromInteger . natVal
 isSigned _   = False
 testBit      = (coerce :: (Natural -> Prelude.Int -> Bool)
                        ->  Word n  -> Prelude.Int -> Bool
                ) testBit
 popCount     = (coerce :: (Natural -> Prelude.Int)
                        ->  Word n  -> Prelude.Int
                ) popCount

trunc :: KnownNat n => Word n -> Word n
trunc = (.&.) maxBound

convBinary :: KnownNat n => CoerceBinary Natural (Word n)
convBinary f = trunc .: coerce f

instance KnownNat n => Num (Word n) where
  (+) = convBinary (+)
  (*) = convBinary (*)
  negate y = (maxBound `xor` y) + 1
  fromInteger = trunc . Word . fromInteger
  abs = id
  signum (Word x) = Word (signum x)

instance KnownNat n => Eq (Word n) where
  (==) = (==) `on` getWord . trunc

instance KnownNat n => Show (Word n) where
  showsPrec n = showsPrec n . getWord . trunc

instance KnownNat n => Ord (Word n) where
  compare = compare `on` getWord . trunc

instance KnownNat n => Real (Word n) where
  toRational = toRational . getWord

instance KnownNat n => Enum (Word n) where
  fromEnum = fromEnum . getWord
  toEnum = trunc . Word . toEnum
  enumFrom x = [x..maxBound]

instance KnownNat n => Integral (Word n) where
  toInteger = toInteger . getWord
  quotRem x y = (convBinary quot x y, convBinary rem x y)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

instance KnownNat n => FiniteBits (Word n) where
  finiteBitSize = fromInteger . natVal

instance KnownNat n => Arbitrary (Word n) where
  arbitrary = arbitraryBoundedEnum

instance (Monad m, KnownNat n) => Serial m (Word n) where
  series = generate (`take` [minBound..maxBound])


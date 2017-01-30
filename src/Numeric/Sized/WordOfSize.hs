{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Numeric.Sized.WordOfSize
  (WordOfSize(..)
  ,allWordsOfSize)
  where

import           Data.Bits
import           Data.Coerce
import           Data.Function
import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits
import           Numeric.Natural

-- | A very small numeric type for exhaustiveness, with wraparound behavior
newtype WordOfSize (n :: Nat) = WordOfSize
    { getWordOfSize :: Natural
    } deriving (Generic)

instance KnownNat n =>
         Bounded (WordOfSize n) where
    minBound = WordOfSize 0
    maxBound = WordOfSize (shift 1 (fromInteger (natVal (Proxy :: Proxy n))) - 1)

type CoerceBinary a b = (a -> a -> a) -> (b -> b -> b)

instance KnownNat n =>
         Bits (WordOfSize n) where
    (.&.) = (coerce :: CoerceBinary Natural (WordOfSize n)) (.&.)
    (.|.) = (coerce :: CoerceBinary Natural (WordOfSize n)) (.|.)
    xor = trunc .: (coerce :: CoerceBinary Natural (WordOfSize n)) xor
    complement =
        trunc . (coerce :: (Natural -> Natural) -> WordOfSize n -> WordOfSize n) complement
    shift =
        trunc .:
        (coerce :: (Natural -> Int -> Natural) -> WordOfSize n -> Int -> WordOfSize n)
            shift
    rotate =
        trunc .:
        (coerce :: (Natural -> Int -> Natural) -> WordOfSize n -> Int -> WordOfSize n)
            rotate
    bit = trunc . WordOfSize . bit
    bitSize = fromInteger . natVal
    bitSizeMaybe = Just . fromInteger . natVal
    isSigned _ = False
    testBit =
        (coerce :: (Natural -> Int -> Bool) -> WordOfSize n -> Int -> Bool)
            testBit
    popCount =
        (coerce :: (Natural -> Int) -> WordOfSize n -> Int) popCount

trunc
    :: KnownNat n
    => WordOfSize n -> WordOfSize n
trunc = (.&.) maxBound

convBinary
    :: KnownNat n
    => CoerceBinary Natural (WordOfSize n)
convBinary f = trunc .: coerce f

instance KnownNat n =>
         Num (WordOfSize n) where
    (+) = convBinary (+)
    (*) = convBinary (*)
    negate y = (maxBound `xor` y) + 1
    fromInteger = trunc . WordOfSize . fromInteger
    abs = id
    signum (WordOfSize x) = WordOfSize (signum x)

instance KnownNat n =>
         Eq (WordOfSize n) where
    (==) = (==) `on` getWordOfSize . trunc

instance KnownNat n =>
         Show (WordOfSize n) where
    showsPrec n = showsPrec n . getWordOfSize . trunc

instance KnownNat n =>
         Ord (WordOfSize n) where
    compare = compare `on` getWordOfSize . trunc

instance KnownNat n =>
         Real (WordOfSize n) where
    toRational = toRational . getWordOfSize

instance KnownNat n =>
         Enum (WordOfSize n) where
    fromEnum = fromEnum . getWordOfSize
    toEnum = trunc . WordOfSize . toEnum
    enumFrom x = [x .. maxBound]

instance KnownNat n =>
         Integral (WordOfSize n) where
    toInteger = toInteger . getWordOfSize
    quotRem x y = (convBinary quot x y, convBinary rem x y)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

instance KnownNat n =>
         FiniteBits (WordOfSize n) where
    finiteBitSize = fromInteger . natVal

allWordsOfSize
    :: KnownNat n
    => [WordOfSize n]
allWordsOfSize = [minBound .. maxBound]

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | This module exports the 'IntOfSize' type and associated functions.
module Numeric.Sized.IntOfSize
  (IntOfSize(..)
  ,allIntsOfSize)
  where

import           Data.Bits
import           Data.Coerce
import           Data.Function
import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits

-- $setup
-- >>> :set -XDataKinds

-- | An integer type with a size decided by a type-level nat. Numeric operations
-- wraparound by default:
--
-- >>> (127 :: IntOfSize 8) + 1
-- -128
newtype IntOfSize (n :: Nat) = IntOfSize
    { getIntOfSize :: Integer
    } deriving (Generic)

instance KnownNat n =>
         Bounded (IntOfSize n) where
    minBound = IntOfSize (shift (-1) (fromInteger (natVal (Proxy :: Proxy n) - 1)))
    maxBound = IntOfSize (shift 1 (fromInteger (natVal (Proxy :: Proxy n) - 1)) - 1)

type CoerceBinary a b = (a -> a -> a) -> (b -> b -> b)

instance KnownNat n =>
         Bits (IntOfSize n) where
    (.&.) = (coerce :: CoerceBinary Integer (IntOfSize n)) (.&.)
    (.|.) = (coerce :: CoerceBinary Integer (IntOfSize n)) (.|.)
    xor = trunc .: (coerce :: CoerceBinary Integer (IntOfSize n)) xor
    complement =
        trunc . (coerce :: (Integer -> Integer) -> IntOfSize n -> IntOfSize n) complement
    shift =
        trunc .:
        (coerce :: (Integer -> Int -> Integer) -> IntOfSize n -> Int -> IntOfSize n)
            shift
    rotate =
        trunc .:
        (coerce :: (Integer -> Int -> Integer) -> IntOfSize n -> Int -> IntOfSize n)
            rotate
    bit = trunc . IntOfSize . bit
    bitSize = fromInteger . natVal
    bitSizeMaybe = Just . fromInteger . natVal
    isSigned _ = True
    testBit =
        (coerce :: (Integer -> Int -> Bool) -> IntOfSize n -> Int -> Bool)
            testBit
    popCount =
        (coerce :: (Integer -> Int) -> IntOfSize n -> Int) popCount

trunc
    :: KnownNat n
    => IntOfSize n -> IntOfSize n
trunc x
  | testBit x (fromInteger (natVal x) - 1) = x .|. minBound
  | otherwise = x .&. maxBound

convBinary
    :: KnownNat n
    => CoerceBinary Integer (IntOfSize n)
convBinary f = trunc .: coerce f

instance KnownNat n =>
         Num (IntOfSize n) where
    (+) = convBinary (+)
    (*) = convBinary (*)
    negate y = complement y + 1
    fromInteger = trunc . IntOfSize . fromInteger
    abs = id
    signum (IntOfSize x) = IntOfSize (signum x)

instance KnownNat n =>
         Eq (IntOfSize n) where
    (==) = (==) `on` getIntOfSize . trunc

instance KnownNat n =>
         Ord (IntOfSize n) where
    compare = compare `on` getIntOfSize . trunc

instance KnownNat n =>
         Real (IntOfSize n) where
    toRational = toRational . getIntOfSize

instance KnownNat n =>
         Enum (IntOfSize n) where
    fromEnum = fromEnum . getIntOfSize
    toEnum = trunc . IntOfSize . toEnum
    enumFrom x = [x .. maxBound]

instance KnownNat n =>
         Integral (IntOfSize n) where
    toInteger = toInteger . getIntOfSize
    quotRem x y = (convBinary quot x y, convBinary rem x y)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

instance KnownNat n =>
         FiniteBits (IntOfSize n) where
    finiteBitSize = fromInteger . natVal

-- | Generate all values, in a sensible order
--
-- >>> allIntsOfSize :: [IntOfSize 4]
-- [0,-1,1,-2,2,-3,3,-4,4,-5,5,-6,6,-7,7,-8]
allIntsOfSize
    :: KnownNat n
    => [IntOfSize n]
allIntsOfSize = f [0 .. maxBound ] (drop 1 [0,-1 .. minBound])
  where
    f (x:xs) ys = x : f ys xs
    f [] ys = ys

instance KnownNat n =>
         Show (IntOfSize n) where
    showsPrec n = showsPrec n . getIntOfSize . trunc

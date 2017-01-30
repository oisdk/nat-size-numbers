{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Numeric.Sized.Int
  (Int(..))
  where

import           Data.Bits
import           Data.Coerce
import           Data.Function
import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits
import           Prelude                hiding (Int)
import qualified Prelude
import           Test.QuickCheck        (Arbitrary (..), arbitraryBoundedEnum)
import           Test.SmallCheck.Series

-- | A very small numeric type for exhaustiveness, with wraparound behavior
newtype Int (n :: Nat) = Int
    { getInt :: Integer
    } deriving (Generic)

instance KnownNat n =>
         Bounded (Int n) where
    minBound = Int (shift (-1) (fromInteger (natVal (Proxy :: Proxy n) - 1)))
    maxBound = Int (shift 1 (fromInteger (natVal (Proxy :: Proxy n) - 1)) - 1)

type CoerceBinary a b = (a -> a -> a) -> (b -> b -> b)

instance KnownNat n =>
         Bits (Int n) where
    (.&.) = (coerce :: CoerceBinary Integer (Int n)) (.&.)
    (.|.) = (coerce :: CoerceBinary Integer (Int n)) (.|.)
    xor = trunc .: (coerce :: CoerceBinary Integer (Int n)) xor
    complement =
        trunc . (coerce :: (Integer -> Integer) -> Int n -> Int n) complement
    shift =
        trunc .:
        (coerce :: (Integer -> Prelude.Int -> Integer) -> Int n -> Prelude.Int -> Int n)
            shift
    rotate =
        trunc .:
        (coerce :: (Integer -> Prelude.Int -> Integer) -> Int n -> Prelude.Int -> Int n)
            rotate
    bit = trunc . Int . bit
    bitSize = fromInteger . natVal
    bitSizeMaybe = Just . fromInteger . natVal
    isSigned _ = True
    testBit =
        (coerce :: (Integer -> Prelude.Int -> Bool) -> Int n -> Prelude.Int -> Bool)
            testBit
    popCount =
        (coerce :: (Integer -> Prelude.Int) -> Int n -> Prelude.Int) popCount

trunc
    :: KnownNat n
    => Int n -> Int n
trunc x
  | testBit x (fromInteger (natVal x) - 1) = x .|. minBound
  | otherwise = x .&. maxBound

convBinary
    :: KnownNat n
    => CoerceBinary Integer (Int n)
convBinary f = trunc .: coerce f

instance KnownNat n =>
         Num (Int n) where
    (+) = convBinary (+)
    (*) = convBinary (*)
    negate y = complement y + 1
    fromInteger = trunc . Int . fromInteger
    abs = id
    signum (Int x) = Int (signum x)

instance KnownNat n =>
         Eq (Int n) where
    (==) = (==) `on` getInt . trunc

instance KnownNat n =>
         Ord (Int n) where
    compare = compare `on` getInt . trunc

instance KnownNat n =>
         Real (Int n) where
    toRational = toRational . getInt

instance KnownNat n =>
         Enum (Int n) where
    fromEnum = fromEnum . getInt
    toEnum = trunc . Int . toEnum
    enumFrom x = [x .. maxBound]

instance KnownNat n =>
         Integral (Int n) where
    toInteger = toInteger . getInt
    quotRem x y = (convBinary quot x y, convBinary rem x y)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

instance KnownNat n =>
         FiniteBits (Int n) where
    finiteBitSize = fromInteger . natVal

instance KnownNat n =>
         Arbitrary (Int n) where
    arbitrary = arbitraryBoundedEnum

instance (KnownNat n, Monad m) =>
         Serial m (Int n) where
    series = generate (`take` vals)
      where
        vals = c [0,-1 .. minBound] [1 .. maxBound]
          where
            c (x:xs) ys = x : c ys xs
            c [] ys     = ys

instance KnownNat n =>
         Show (Int n) where
    showsPrec n = showsPrec n . getInt . trunc

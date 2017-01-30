{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Main (main) where

import           Data.Function
import           Data.Int           hiding (Int)
import           Data.Proxy
import           Data.Word          hiding (Word)
import           GHC.TypeLits
import           Numeric.Natural
import           Numeric.Sized.Int
import           Numeric.Sized.Word
import           Prelude            hiding (Int, Word)
import           Test.QuickCheck

type family IntType (n :: Nat) :: * where
  IntType 8  = Int8
  IntType 16 = Int16
  IntType 32 = Int32
  IntType 64 = Int64

type family WordType (n :: Nat) :: * where
  WordType 8  = Word8
  WordType 16 = Word16
  WordType 32 = Word32
  WordType 64 = Word64

sameConvAs :: (Integral n, Integral m, Integral i, Show i) => Proxy n -> Proxy m -> i -> Property
sameConvAs (_ :: Proxy n) (_ :: Proxy m) (x :: i)
  = ((fromIntegral :: n -> i) . (fromIntegral :: i -> n)) x
  === ((fromIntegral :: m -> i) . (fromIntegral :: i -> m)) x

sameConvI :: (KnownNat n, Integral (IntType n)) => Proxy n -> Integer -> Property
sameConvI (_ :: Proxy n) = sameConvAs (Proxy :: Proxy (Int n)) (Proxy :: Proxy (IntType n))

sameConvW :: (KnownNat n, Integral (WordType n)) => Proxy n -> Natural -> Property
sameConvW (_ :: Proxy n) = sameConvAs (Proxy :: Proxy (Word n)) (Proxy :: Proxy (WordType n))

sameFncAs :: (Integral n, Integral m, Integral i, Show i) => (forall a. Integral a => a -> a -> a) -> Proxy n -> Proxy m -> i -> i -> Property
sameFncAs f (_ :: Proxy n) (_ :: Proxy m) (x :: i) (y :: i)
  = (fromIntegral :: n -> i) ((f `on` (fromIntegral :: i -> n)) x y)
  === (fromIntegral :: m -> i) ((f `on` (fromIntegral :: i -> m)) x y)

sameFncI :: (KnownNat n, Integral (IntType n)) => (forall a. Integral a => a -> a -> a) -> Proxy n -> Integer -> Integer -> Property
sameFncI f (_ :: Proxy n) = sameFncAs f (Proxy :: Proxy (Int n)) (Proxy :: Proxy (IntType n))

sameFncW :: (KnownNat n, Integral (WordType n)) => (forall a. Integral a => a -> a -> a) -> Proxy n -> Natural -> Natural -> Property
sameFncW f (_ :: Proxy n) = sameFncAs f (Proxy :: Proxy (Word n)) (Proxy :: Proxy (WordType n))

testAll :: Testable a => (forall n. (KnownNat n, Integral (WordType n), Integral (IntType n)) => Proxy n -> a) -> IO ()
testAll prop = do
  quickCheck (prop (Proxy :: Proxy 8))
  quickCheck (prop (Proxy :: Proxy 16))
  quickCheck (prop (Proxy :: Proxy 32))
  quickCheck (prop (Proxy :: Proxy 64))

main :: IO ()
main = do
  testAll sameConvI
  testAll sameConvW
  testAll (sameFncI (+))
  testAll (sameFncW (+))
  testAll (sameFncI (*))
  testAll (sameFncW (*))
  testAll (sameFncI (-))
  testAll (sameFncW (-))

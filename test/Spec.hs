{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
  (main)
  where

import           Data.Function
import           Data.Int
import           Data.Proxy
import           Data.Word
import           GHC.TypeLits
import           Numeric.Natural
import           Numeric.Sized.IntOfSize
import           Numeric.Sized.WordOfSize
import           Test.DocTest
import           Test.QuickCheck          hiding (generate)
import qualified Test.SmallCheck          as SmallCheck
import           Test.SmallCheck.Series

instance KnownNat n =>
         Arbitrary (IntOfSize n) where
    arbitrary = arbitraryBoundedEnum

instance KnownNat n =>
         Arbitrary (WordOfSize n) where
    arbitrary = arbitraryBoundedEnum

instance (KnownNat n, Monad m) =>
         Serial m (IntOfSize n) where
    series = generate (`take` allIntsOfSize)

instance (KnownNat n, Monad m) =>
         Serial m (WordOfSize n) where
    series = generate (`take` allWordsOfSize)

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

sameConvAs
    :: (Integral n, Integral m, Integral i, Show i)
    => Proxy n -> Proxy m -> i -> Property
sameConvAs (_ :: Proxy n) (_ :: Proxy m) (x :: i) =
    ((fromIntegral :: n -> i) . (fromIntegral :: i -> n)) x ===
    ((fromIntegral :: m -> i) . (fromIntegral :: i -> m)) x

sameConvI
    :: (KnownNat n, Integral (IntType n))
    => Proxy n -> Integer -> Property
sameConvI (_ :: Proxy n) =
    sameConvAs (Proxy :: Proxy (IntOfSize n)) (Proxy :: Proxy (IntType n))

sameConvW
    :: (KnownNat n, Integral (WordType n))
    => Proxy n -> Natural -> Property
sameConvW (_ :: Proxy n) =
    sameConvAs (Proxy :: Proxy (WordOfSize n)) (Proxy :: Proxy (WordType n))

sameFncAs
    :: (Integral n, Integral m, Integral i, Show i)
    => (forall a. Integral a =>
                  a -> a -> a)
    -> Proxy n
    -> Proxy m
    -> i
    -> i
    -> Property
sameFncAs f (_ :: Proxy n) (_ :: Proxy m) (x :: i) (y :: i) =
    (fromIntegral :: n -> i) ((f `on` (fromIntegral :: i -> n)) x y) ===
    (fromIntegral :: m -> i) ((f `on` (fromIntegral :: i -> m)) x y)

sameFncI, sameFncINZRhs
    :: (KnownNat n, Integral (IntType n))
    => (forall a. Integral a =>
                  a -> a -> a)
    -> Proxy n
    -> Integer
    -> Integer
    -> Property
sameFncI f (_ :: Proxy n) =
    sameFncAs f (Proxy :: Proxy (IntOfSize n)) (Proxy :: Proxy (IntType n))

sameFncINZRhs f n x y = y /= 0 ==> sameFncI f n x y

sameFncW, sameFncWNZRhs
    :: (KnownNat n, Integral (WordType n))
    => (forall a. Integral a =>
                  a -> a -> a)
    -> Proxy n
    -> Natural
    -> Natural
    -> Property
sameFncW f (_ :: Proxy n) =
    sameFncAs f (Proxy :: Proxy (WordOfSize n)) (Proxy :: Proxy (WordType n))

sameFncWNZRhs f n x y = y /= 0 ==> sameFncW f n x y

sameFncAsS
    :: (Integral n, Integral m, Show n)
    => (forall a. Integral a =>
                  a -> a -> a)
    -> Proxy n
    -> Proxy m
    -> n
    -> n
    -> Either String String
sameFncAsS f (_ :: Proxy n) (_ :: Proxy m) x y =
    if (fromIntegral :: n -> m) (f x y) ==
       (f `on` (fromIntegral :: n -> m)) x y
        then Right ""
        else Left (show x ++ " " ++ show y)

sameFncWS
    :: (KnownNat n, Integral (WordType n))
    => (forall a. Integral a =>
                  a -> a -> a)
    -> Proxy n
    -> WordOfSize n
    -> WordOfSize n
    -> Either String String
sameFncWS f (_ :: Proxy n) =
    sameFncAsS f (Proxy :: Proxy (WordOfSize n)) (Proxy :: Proxy (WordType n))

sameFncWNZRhsS
    :: (KnownNat n, Integral (WordType n), Monad m)
    => (forall a. Integral a =>
                  a -> a -> a)
    -> Proxy n
    -> WordOfSize n
    -> WordOfSize n
    -> SmallCheck.Property m
sameFncWNZRhsS f n x y = y /= 0 SmallCheck.==> sameFncWS f n x y

sameFncIS
    :: (KnownNat n, Integral (IntType n))
    => (forall a. Integral a =>
                  a -> a -> a)
    -> Proxy n
    -> IntOfSize n
    -> IntOfSize n
    -> Either String String
sameFncIS f (_ :: Proxy n) =
    sameFncAsS f (Proxy :: Proxy (IntOfSize n)) (Proxy :: Proxy (IntType n))

sameFncINZRhsS
    :: (KnownNat n, Integral (IntType n), Monad m, Bounded (IntOfSize n))
    => (forall a. Integral a =>
                  a -> a -> a)
    -> Proxy n
    -> IntOfSize n
    -> IntOfSize n
    -> SmallCheck.Property m
sameFncINZRhsS f n x y =
    y /= 0 && (x /= minBound || y /= -1) SmallCheck.==> sameFncIS f n x y

testAll
    :: Testable a
    => (forall n. (KnownNat n, Integral (WordType n), Integral (IntType n)) =>
                  Proxy n -> a)
    -> IO ()
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
    testAll (sameFncINZRhs div)
    testAll (sameFncWNZRhs div)
    testAll (sameFncINZRhs mod)
    testAll (sameFncWNZRhs mod)
    testAll (sameFncINZRhs rem)
    testAll (sameFncWNZRhs rem)
    testAll (sameFncINZRhs quot)
    testAll (sameFncWNZRhs quot)
    SmallCheck.smallCheck 100000 (sameFncWS (+) (Proxy :: Proxy 8))
    SmallCheck.smallCheck 100000 (sameFncIS (+) (Proxy :: Proxy 8))
    SmallCheck.smallCheck 100000 (sameFncWS (*) (Proxy :: Proxy 8))
    SmallCheck.smallCheck 100000 (sameFncIS (*) (Proxy :: Proxy 8))
    SmallCheck.smallCheck 100000 (sameFncWS (-) (Proxy :: Proxy 8))
    SmallCheck.smallCheck 100000 (sameFncIS (-) (Proxy :: Proxy 8))
    SmallCheck.smallCheck 100000 (sameFncWNZRhsS div (Proxy :: Proxy 8))
    SmallCheck.smallCheck 100000 (sameFncINZRhsS div (Proxy :: Proxy 8))
    SmallCheck.smallCheck 100000 (sameFncWNZRhsS mod (Proxy :: Proxy 8))
    SmallCheck.smallCheck 100000 (sameFncINZRhsS mod (Proxy :: Proxy 8))
    SmallCheck.smallCheck 100000 (sameFncWNZRhsS rem (Proxy :: Proxy 8))
    SmallCheck.smallCheck 100000 (sameFncINZRhsS rem (Proxy :: Proxy 8))
    SmallCheck.smallCheck 100000 (sameFncWNZRhsS quot (Proxy :: Proxy 8))
    SmallCheck.smallCheck 100000 (sameFncINZRhsS quot (Proxy :: Proxy 8))
    doctest ["-isrc", "src/"]

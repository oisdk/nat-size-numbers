{-# LANGUAGE ScopedTypeVariables, DataKinds #-}

module Main (main) where

import Test.QuickCheck
import Test.DocTest
import Numeric.Sized.Word
import Data.Proxy
import GHC.TypeLits
import Prelude hiding (Word)
import Data.Word hiding (Word)

sameConvAs :: (KnownNat n, Integral m) => Proxy n -> Proxy m -> Integer -> Property
sameConvAs (_ :: Proxy n) (_ :: Proxy m) x
  = ((fromIntegral :: Word n -> Integer) . (fromInteger :: Integer -> Word n)) x
  === ((fromIntegral :: m -> Integer) . (fromInteger :: Integer -> m)) x


main :: IO ()
main = do
  quickCheck (sameConvAs (Proxy :: Proxy 8) (Proxy :: Proxy Word8))
  doctest
    [ "-isrc"
    , "src/" ]

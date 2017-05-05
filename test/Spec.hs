{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Hedgehog
import qualified Hedgehog.Gen       as Gen
import qualified Hedgehog.Range     as Range
import           Test.DocTest

import           Numeric.Sized.WordOfSize
import           Numeric.Sized.IntOfSize

import           Control.Monad

import           Data.Data

default ()

binaryProp
    :: forall a.
       Integral a
    => (forall t. Integral t => t -> t -> t)
    -> Integer
    -> Integer
    -> (Integer -> Integer -> Bool)
    -> Property
binaryProp op lb ub cond = property $ do
    x <- forAll (Gen.integral (Range.linear lb ub))
    y <- forAll (Gen.integral (Range.linear lb ub))
    guard (cond x y)
    let zb = op x y
    let zt = op (fromInteger x :: a) (fromInteger y)
    zb === toInteger zt

ordProps
    :: forall a.
       (Ord a, Show a, Typeable a)
    => Gen IO a
    -> Property
ordProps xs = property $ do
    x <- forAll xs
    info "reflexive"
    x === x
    info "irreflexive"
    assert (not (x < x))
    y <- forAll xs
    info "Ord functions behave same as default implementations"
    case compare x y of
      LT -> do
          assert (x < y)
          assert (x /= y)
          assert (not (x == y))
          assert (x <= y)
          assert (not (x >= y))

          info "antisymmetric"
          assert (y > x)
          info "irreflexive"
          assert (not (x > y))
          info "transitive"
          z <- forAll xs
          when (z > y) (assert (z > x))
      EQ -> do
          assert (x == y)
          assert (not (x /= y))
          assert (not (x < y))
          assert (not (x > y))
          assert (x <= y)
          assert (x >= y)

          info "symmetric"
          assert (y == x)
          info "transitive"
          z <- forAll xs
          assert $ (x == z) == (y == z)
      GT -> do
          assert (x > y)
          assert (x /= y)
          assert (not (x == y))
          assert (x >= y)
          assert (not (x <= y))

          info "irreflexive"
          assert (not (x < y))
          info "antisymmetric"
          assert (y < x)
          info "transitive"
          z <- forAll xs
          when (z < y) (assert (z < x))

holdsForLength :: Foldable f => (a -> Bool) -> f a -> Int
holdsForLength p = flip (foldr f id ) 0 where
  f e a i | p e = a (i + 1)
          | otherwise = i

enumProps
    :: forall a.
       (Enum a, Show a, Typeable a, Ord a)
    => (Int -> Bool) -> Gen IO Int -> Gen IO a -> Property
enumProps p ig eg = property $ do
    x <- forAll ig
    info "from . to"
    (fromEnum . toEnum @a) x === x
    info "to . from"
    n <- forAll eg
    (toEnum . fromEnum) n === n
    info "[n..]"
    let lhs1 = take 100 $ map fromEnum [n..]
        rhs1 = take 100 $ [fromEnum n..]
        len1 = min (holdsForLength p lhs1) (holdsForLength p rhs1)
    take len1 lhs1 === take len1 rhs1
    info "[n,m..]"
    m <- forAll eg
    let lhs2 = take 100 $ map fromEnum [n,m..]
        rhs2 = take 100 $ [fromEnum n, fromEnum m..]
        len2 = min (holdsForLength p lhs2) (holdsForLength p rhs2)
    take len2 lhs2 === take len2 rhs2
    when (m >= n) $ do
        info "[n..m]"
        map fromEnum [n..m] === [fromEnum n..fromEnum m]
    l <- forAll eg
    when (((l > n) == (n > m)) && (l /= n)) $ do
        info "[l,n..m]"
        map fromEnum [l,n..m] === [fromEnum l, fromEnum n..fromEnum m]


prop_Word3Add :: Property
prop_Word3Add = binaryProp @(WordOfSize 3) (+) 0 7 (\x y -> x + y <= 7)

prop_Word3Mul :: Property
prop_Word3Mul = binaryProp @(WordOfSize 3) (*) 0 7 (\x y -> x * y <= 7)

prop_Word3Sub :: Property
prop_Word3Sub = withDiscards 1000 $ binaryProp @(WordOfSize 3) (-) 0 7 (>=)

prop_Word3Rem :: Property
prop_Word3Rem = binaryProp @(WordOfSize 3) rem 0 7 (\_ y -> y > 0)

prop_Word3Quot :: Property
prop_Word3Quot = binaryProp @(WordOfSize 3) quot 0 7 (\_ y -> y > 0)

prop_Word3Ord :: Property
prop_Word3Ord = ordProps (Gen.integral (Range.linear @(WordOfSize 3) 0 7))

prop_Word3Enum :: Property
prop_Word3Enum =
    enumProps
        (inBounds 0 7)
        (Gen.integral (Range.linear 0 7))
        (Gen.integral (Range.linear @(WordOfSize 3) 0 7))

inBounds :: Ord a => a -> a -> a -> Bool
inBounds lb ub x = x >= lb && x <= ub

prop_Int3Add :: Property
prop_Int3Add =
    withDiscards 1000 $
    binaryProp
        @(IntOfSize 3)
        (+)
        (-4)
        3
        (\x y ->
              inBounds (-4) 3 (x + y))

prop_Int3Mul :: Property
prop_Int3Mul =
    withDiscards 1000 $
    binaryProp
        @(IntOfSize 3)
        (*)
        (-4)
        3
        (\x y ->
              inBounds (-4) 3 (x * y))

prop_Int3Sub :: Property
prop_Int3Sub = binaryProp @(IntOfSize 3) (-) (-4) 3 (\x y -> inBounds (-4) 3 (x - y))

prop_Int3Rem :: Property
prop_Int3Rem = binaryProp @(IntOfSize 3) rem (-4) 3 (\_ y -> y /= 0)

prop_Int3Quot :: Property
prop_Int3Quot = binaryProp @(IntOfSize 3) quot (-4) 3 (\x y -> y /= 0 && inBounds (-4) 3 (quot x y))

prop_Int3Ord :: Property
prop_Int3Ord = ordProps (Gen.integral (Range.linear @(IntOfSize 3) (-3) 4))

prop_Int3Enum :: Property
prop_Int3Enum =
    enumProps
        (inBounds (-4) 3)
        (Gen.integral (Range.linear (-4) 3))
        (Gen.integral (Range.linear @(IntOfSize 3) (-4) 3))

main :: IO Bool
main = do
    doctest ["-isrc","src/"]
    $$(checkConcurrent)

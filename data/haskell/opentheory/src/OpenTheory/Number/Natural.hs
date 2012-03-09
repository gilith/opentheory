{- |
Module: $Header$
Description: A natural number type
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable

A natural number type
-}
module OpenTheory.Number.Natural
  ( Natural,
    equal )
where

import Test.QuickCheck

newtype Natural =
    Natural { unNatural :: Integer }
  deriving (Eq,Show)

instance Arbitrary Natural where
  arbitrary = fmap fromInt arbitrary
      where
    fromInt :: Integer -> Natural
    fromInt x = Natural (if 0 <= x then x else -(x + 1))

instance Ord Natural where
  compare x y = compare (unNatural x) (unNatural y)

instance Num Natural where
  x + y = Natural (unNatural x + unNatural y)

  x * y = Natural (unNatural x * unNatural y)

  abs x = x

  signum x = if unNatural x == 0 then x else Natural 1

  fromInteger x =
      if 0 <= x
        then Natural x
        else error "OpenTheory.Number.Natural.fromInteger"

equal :: Natural -> Natural -> Bool
equal n1 n2 = n1 == n2

{- |
module: $Header$
description: Primitive natural number functions
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Primitive.Natural
  ( Natural,
    shiftLeft,
    shiftRight )
where

import Data.Bits
import qualified Test.QuickCheck

newtype Natural =
    Natural { unNatural :: Integer }
  deriving (Eq, Ord)

shiftLeft :: Natural -> Natural -> Natural
shiftLeft (Natural x) k = Natural (shiftL x (fromIntegral k))

shiftRight :: Natural -> Natural -> Natural
shiftRight (Natural x) k = Natural (shiftR x (fromIntegral k))

instance Show Natural where
  show x = show (unNatural x)

instance Num Natural where
  x + y = Natural (unNatural x + unNatural y)

  x - y =
      if x < y
        then error "OpenTheory.Primitive.Natural.-"
        else Natural (unNatural x - unNatural y)

  x * y = Natural (unNatural x * unNatural y)

  abs x = x

  signum x = if unNatural x == 0 then x else Natural 1

  fromInteger x =
      if x < 0
        then error "OpenTheory.Primitive.Natural.fromInteger"
        else Natural x

instance Real Natural where
  toRational x = toRational (unNatural x)

instance Enum Natural where
  toEnum x =
      if x < 0
        then error "OpenTheory.Primitive.Natural.toEnum"
        else Natural (toEnum x)

  fromEnum x = fromEnum (unNatural x)

instance Integral Natural where
  divMod x y =
      if y == 0
        then error "OpenTheory.Primitive.Natural.divMod"
        else
          let (d,m) = divMod (unNatural x) (unNatural y)
          in (Natural d, Natural m)

  quotRem x y =
      if y == 0
        then error "OpenTheory.Primitive.Natural.quotRem"
        else
          let (q,r) = quotRem (unNatural x) (unNatural y)
          in (Natural q, Natural r)

  toInteger = unNatural

instance Data.Bits.Bits Natural where
  x .&. y = Natural (unNatural x .&. unNatural y)

  x .|. y = Natural (unNatural x .|. unNatural y)

  xor x y = Natural (xor (unNatural x) (unNatural y))

  complement _ = error "OpenTheory.Primitive.Natural.complement"

  shift x k = Natural (shift (unNatural x) k)

  shiftL x k = Natural (shiftL (unNatural x) k)

  shiftR x k = Natural (shiftR (unNatural x) k)

  rotate _ _ = error "OpenTheory.Primitive.Natural.rotate"

  bitSize _ = error "OpenTheory.Primitive.Natural.bitSize"

  isSigned _ = False

  testBit x k = testBit (unNatural x) k

  bit k = Natural (bit k)

  popCount x = popCount (unNatural x)

instance Test.QuickCheck.Arbitrary Natural where
  arbitrary = fmap fromRandomInteger Test.QuickCheck.arbitrary
      where
    fromRandomInteger x =
      Natural (if x < 0 then -(x + 1) else x)

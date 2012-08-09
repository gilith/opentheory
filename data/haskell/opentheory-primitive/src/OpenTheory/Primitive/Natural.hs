{- |
Module: $Header$
Description: A natural number type
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable

A natural number type
-}
module OpenTheory.Primitive.Natural
  ( Natural )
where

newtype Natural =
    Natural { unNatural :: Integer }
  deriving Eq

instance Show Natural where
  show n = show (unNatural n)

instance Ord Natural where
  compare x y = compare (unNatural x) (unNatural y)

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

{- |
Module: $Header$
Description: A natural number type
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable

A natural number type
-}
module OpenTheory.Primitive.Number.Natural
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
      if y <= x
        then Natural (unNatural x - unNatural y)
        else error "OpenTheory.Primitive.Number.Natural.-"

  x * y = Natural (unNatural x * unNatural y)

  abs x = x

  signum x = if unNatural x == 0 then x else Natural 1

  fromInteger x =
      if 0 <= x
        then Natural x
        else error "OpenTheory.Primitive.Number.Natural.fromInteger"

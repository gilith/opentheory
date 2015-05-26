{- |
module: CRT
description: A natural number implementation of the Chinese Remainder Theorem
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module CRT
where

import OpenTheory.Primitive.Natural

import Egcd

integerCRT :: Integer -> Integer -> Integer -> Integer -> Integer
integerCRT a b =
    \x y -> (x * tb + y * sa) `mod` ab
  where
    (_,s,t) = integerEgcd a b
    ab = a * b
    sa = s * a
    tb = t * b

naturalCRT :: Natural -> Natural -> Natural -> Natural -> Natural
naturalCRT a b =
    \x y -> (x * tb + y * sa) `mod` ab
  where
    (_,s,t) = naturalEgcd a b
    ab = a * b
    sa = s * a
    tb = (a - t) * b

{- |
module: Montgomery
description: Modular exponentiation using Montgomery multiplication
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Montgomery
where

import OpenTheory.Primitive.Natural
import qualified OpenTheory.Natural.Bits as Bits

import Egcd

data Montgomery = Montgomery
    {rMontgomery :: Natural,
     sMontgomery :: Natural,
     kMontgomery :: Natural,
     nMontgomery :: Natural,
     tMontgomery :: Natural}
  deriving Show

type MontgomeryNatural = Natural

standard :: Natural -> Montgomery
standard n =
    Montgomery
      {nMontgomery = n,
       rMontgomery = r,
       sMontgomery = s,
       kMontgomery = k,
       tMontgomery = t}
  where
    r = Bits.width n

    r2 = shiftLeft 1 r

    (_,s,k) = naturalEgcd r2 n

    t = r2 `mod` n

reduce :: Montgomery -> Natural -> Natural
reduce m a =
    shiftRight (a + Bits.bound (a * k) r * n) r
  where
    r = rMontgomery m
    k = kMontgomery m
    n = nMontgomery m

fromNatural :: Montgomery -> Natural -> MontgomeryNatural
fromNatural m a =
    shiftLeft a r `mod` n
  where
    r = rMontgomery m
    n = nMontgomery m

toNatural :: Montgomery -> MontgomeryNatural -> Natural
toNatural m a =
    reduce m a `mod` n
  where
    n = nMontgomery m

normalize :: Montgomery -> MontgomeryNatural -> MontgomeryNatural
normalize m =
    loop
  where
    loop a =
        if x == 0 then a else loop ((a - shiftLeft x r) + x * t)
      where
        x = shiftRight a r

    r = rMontgomery m

    t = tMontgomery m

add :: Montgomery -> MontgomeryNatural -> MontgomeryNatural -> MontgomeryNatural
add m a b = normalize m (a + b)

multiply ::
    Montgomery -> MontgomeryNatural -> MontgomeryNatural -> MontgomeryNatural
multiply m a b =
    if Bits.bit c r then c - n else c
  where
    c = reduce m (a * b)

    r = rMontgomery m

    n = nMontgomery m

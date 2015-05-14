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
import qualified ModExp

data Montgomery = Montgomery
    {nMontgomery :: Natural,
     wMontgomery :: Natural,
     sMontgomery :: Natural,
     kMontgomery :: Natural,
     rMontgomery :: Natural,
     r2Montgomery :: Natural}
  deriving Show

type NaturalM = Natural

standard :: Natural -> Montgomery
standard n =
    Montgomery
      {nMontgomery = n,
       wMontgomery = w,
       sMontgomery = s,
       kMontgomery = k,
       rMontgomery = r,
       r2Montgomery = r2}
  where
    w = Bits.width n
    w2 = shiftLeft 1 w
    (_,s,k) = naturalEgcd w2 n
    r = w2 `mod` n
    r2 = (r * r) `mod` n

oneM :: Montgomery -> NaturalM
oneM = rMontgomery

reduce :: Montgomery -> Natural -> Natural
reduce m a =
    shiftRight (a + Bits.bound (a * k) w * n) w
  where
    n = nMontgomery m
    w = wMontgomery m
    k = kMontgomery m

normalize :: Montgomery -> NaturalM -> NaturalM
normalize m =
    loop
  where
    loop a =
        if x == 0 then a else loop ((a - shiftLeft x w) + x * r)
      where
        x = shiftRight a w

    w = wMontgomery m
    r = rMontgomery m

fromNatural :: Montgomery -> Natural -> NaturalM
fromNatural m a =
    normalize m (reduce m (a * r2))
  where
    r2 = r2Montgomery m

toNatural :: Montgomery -> NaturalM -> Natural
toNatural m a =
    if b < n then b else 0
  where
    b = reduce m a
    n = nMontgomery m

addM :: Montgomery -> NaturalM -> NaturalM -> NaturalM
addM m a b = normalize m (a + b)

multiplyM :: Montgomery -> NaturalM -> NaturalM -> NaturalM
multiplyM m a b =
    if Bits.bit c w then c - n else c
  where
    c = reduce m (a * b)
    n = nMontgomery m
    w = wMontgomery m

squareM :: Montgomery -> NaturalM -> NaturalM
squareM m a = multiplyM m a a

modExpM :: Montgomery -> NaturalM -> Natural -> NaturalM
modExpM m = ModExp.multiplyExponential (multiplyM m) (oneM m)

modDoubleExpM :: Montgomery -> NaturalM -> Natural -> NaturalM
modDoubleExpM m = ModExp.functionPower (squareM m)

modExp :: Natural -> Natural -> Natural -> Natural
modExp n x k =
    toNatural m (modExpM m (fromNatural m x) k)
  where
    m = standard n

modDoubleExp :: Natural -> Natural -> Natural -> Natural
modDoubleExp n x k =
    toNatural m (modDoubleExpM m (fromNatural m x) k)
  where
    m = standard n

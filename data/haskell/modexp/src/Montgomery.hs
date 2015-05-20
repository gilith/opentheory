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

import qualified Egcd
import qualified Modexp

data Montgomery = Montgomery
    {nMontgomery :: Natural,
     wMontgomery :: Natural,
     sMontgomery :: Natural,
     kMontgomery :: Natural,
     rMontgomery :: Natural,
     r2Montgomery :: Natural}
  deriving Show

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
    (_,s,k) = Egcd.naturalEgcd w2 n
    r = w2 `mod` n
    r2 = (r * r) `mod` n

-- normalize m a `mod` n = a `mod` n
-- normalize m a < 2 ^ w
normalize :: Montgomery -> Natural -> Natural
normalize m =
    loop
  where
    loop a =
        if x == 0 then a else loop ((a - shiftLeft x w) + x * r)
      where
        x = shiftRight a w

    w = wMontgomery m
    r = rMontgomery m

-- reduce m a `mod` n = (a * s) `mod` n
-- a <= r * x ==> reduce m a < x + n
reduce :: Montgomery -> Natural -> Natural
reduce m a =
    shiftRight (a + Bits.bound (a * k) w * n) w
  where
    n = nMontgomery m
    w = wMontgomery m
    k = kMontgomery m

toNatural :: Montgomery -> Natural -> Natural
toNatural m a =
    if b < n then b else 0
  where
    b = reduce m a
    n = nMontgomery m

fromNatural :: Montgomery -> Natural -> Natural
fromNatural m a =
    multiplyM m (normalize m a) r2
  where
    r2 = r2Montgomery m

oneM :: Montgomery -> Natural
oneM = rMontgomery

addM :: Montgomery -> Natural -> Natural -> Natural
addM m a b = normalize m (a + b)

multiplyM :: Montgomery -> Natural -> Natural -> Natural
multiplyM m a b =
    if Bits.bit c w then c - n else c
  where
    c = reduce m (a * b)
    n = nMontgomery m
    w = wMontgomery m

squareM :: Montgomery -> Natural -> Natural
squareM m a = multiplyM m a a

expM :: Montgomery -> Natural -> Natural -> Natural
expM m = Modexp.multiplyExponential (multiplyM m) (oneM m)

exp2M :: Montgomery -> Natural -> Natural -> Natural
exp2M m x k = Modexp.functionPower (squareM m) k x

modexp :: Natural -> Natural -> Natural -> Natural
modexp n x k =
    toNatural m (expM m (fromNatural m x) k)
  where
    m = standard n

modexp2 :: Natural -> Natural -> Natural -> Natural
modexp2 n x k =
    toNatural m (exp2M m (fromNatural m x) k)
  where
    m = standard n

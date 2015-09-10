{- |
module: Arithmetic.Montgomery
description: Modular arithmetic using Montgomery multiplication
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.Montgomery
where

import OpenTheory.Primitive.Natural
import qualified OpenTheory.Natural.Bits as Bits
import OpenTheory.Natural.Divides

import Arithmetic.Utility

data Parameters = Parameters
    {nParameters :: Natural,
     wParameters :: Natural,
     sParameters :: Natural,
     kParameters :: Natural,
     rParameters :: Natural,
     r2Parameters :: Natural,
     zParameters :: Natural}
  deriving Show

data Montgomery = Montgomery
    {pMontgomery :: Parameters,
     nMontgomery :: Natural}
  deriving Show

align :: Natural -> Natural -> Natural
align b n = if n == 0 then 0 else (((n - 1) `div` b) + 1) * b

customParameters :: Natural -> Natural -> Parameters
customParameters n w =
    Parameters
      {nParameters = n,
       wParameters = w,
       sParameters = s,
       kParameters = k,
       rParameters = r,
       r2Parameters = r2,
       zParameters = z}
  where
    w2 = shiftLeft 1 w
    (_,(s,k)) = egcd w2 n
    r = w2 `mod` n
    r2 = (r * r) `mod` n
    z = w2 + n - r

alignedParameters :: Natural -> Natural -> Parameters
alignedParameters b n = customParameters n (align b (Bits.width n))

standardParameters :: Natural -> Parameters
standardParameters = alignedParameters 64

-- normalize p a `mod` n = a `mod` n
-- normalize p a < 2 ^ w
normalize :: Parameters -> Natural -> Montgomery
normalize p =
    loop
  where
    w = wParameters p
    r = rParameters p

    loop a =
        if x == 0 then
          Montgomery
            {pMontgomery = p,
             nMontgomery = a}
        else
          loop ((a - shiftLeft x w) + x * r)
      where
        x = shiftRight a w

-- normalize1 p a `mod` n = a `mod` n
-- a < 2 ^ w + n ==> normalize1 p a < 2 ^ w
normalize1 :: Parameters -> Natural -> Montgomery
normalize1 p a =
    Montgomery {pMontgomery = p, nMontgomery = b}
  where
    n = nParameters p
    w = wParameters p
    b = if Bits.bit a w then a - n else a

-- reduce p a `mod` n = (a * s) `mod` n
-- a <= r * x ==> reduce p a < x + n
reduce :: Parameters -> Natural -> Natural
reduce p a =
    shiftRight (a + Bits.bound (a * k) w * n) w
  where
    n = nParameters p
    w = wParameters p
    k = kParameters p

toNatural :: Montgomery -> Natural
toNatural a =
    if b < n then b else 0
  where
    p = pMontgomery a
    n = nParameters p
    b = reduce p (nMontgomery a)

fromNatural :: Parameters -> Natural -> Montgomery
fromNatural p =
    multiply r2 . normalize p
  where
    r2 = Montgomery {pMontgomery = p, nMontgomery = r2Parameters p}

zero :: Parameters -> Montgomery
zero p = Montgomery {pMontgomery = p, nMontgomery = 0}

one :: Parameters -> Montgomery
one p = Montgomery {pMontgomery = p, nMontgomery = rParameters p}

two :: Parameters -> Montgomery
two p = double (one p)

add :: Montgomery -> Montgomery -> Montgomery
add a b = normalize (pMontgomery a) (nMontgomery a + nMontgomery b)

double :: Montgomery -> Montgomery
double a = add a a

negate :: Montgomery -> Montgomery
negate a =
    normalize1 p (z - nMontgomery a)
  where
    p = pMontgomery a
    z = zParameters p

subtract :: Montgomery -> Montgomery -> Montgomery
subtract a b = add a (Arithmetic.Montgomery.negate b)

multiply :: Montgomery -> Montgomery -> Montgomery
multiply a b =
    normalize1 p (reduce p (nMontgomery a * nMontgomery b))
  where
    p = pMontgomery a

square :: Montgomery -> Montgomery
square a = multiply a a

exp :: Montgomery -> Natural -> Montgomery
exp a =
    multiplyExponential multiply (one p) a
  where
    p = pMontgomery a

exp2 :: Montgomery -> Natural -> Montgomery
exp2 a k = functionPower square k a

modexp :: Natural -> Natural -> Natural -> Natural
modexp n a k =
    toNatural m
  where
    p = standardParameters n
    m = Arithmetic.Montgomery.exp (fromNatural p a) k

modexp2 :: Natural -> Natural -> Natural -> Natural
modexp2 n a k =
    toNatural m
  where
    p = standardParameters n
    m = exp2 (fromNatural p a) k

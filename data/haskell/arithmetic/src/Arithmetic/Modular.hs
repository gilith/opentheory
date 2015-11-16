{- |
module: Arithmetic.Modular
description: Modular arithmetic
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.Modular
where

import OpenTheory.Primitive.Natural
import qualified OpenTheory.Natural.Divides as Divides

import Arithmetic.Utility
import qualified Arithmetic.Ring as Ring

normalize :: Natural -> Natural -> Natural
normalize n x = x `mod` n

add :: Natural -> Natural -> Natural -> Natural
add n x y = normalize n (x + y)

negate :: Natural -> Natural -> Natural
negate n x =
    if y == 0 then y else n - y
  where
    y = normalize n x

multiply :: Natural -> Natural -> Natural -> Natural
multiply n x y = normalize n (x * y)

divide :: Natural -> Natural -> Natural -> Maybe Natural
divide n x y =
    if g == n then if Divides.divides n x then Just 0 else Nothing
    else if Divides.divides g x then Just (multiply n (x `div` g) s)
    else Nothing
  where
    (g,(s,_)) = Divides.egcd y n  -- s * y == g (mod n)

ring :: Natural -> Ring.Ring Natural
ring n =
    Ring.Ring {Ring.fromNatural = normalize n,
               Ring.add = add n,
               Ring.negate = Arithmetic.Modular.negate n,
               Ring.multiply = multiply n,
               Ring.divide = divide n}

double :: Natural -> Natural -> Natural
double = Ring.double . ring

subtract :: Natural -> Natural -> Natural -> Natural
subtract n x y =
    if y <= x then normalize n (x - y)
    else Arithmetic.Modular.negate n (y - x)

square :: Natural -> Natural -> Natural
square = Ring.square . ring

exp :: Natural -> Natural -> Natural -> Natural
exp = Ring.exp . ring

exp2 :: Natural -> Natural -> Natural -> Natural
exp2 n x k = if k == 0 then normalize n x else functionPower (square n) k x

invert :: Natural -> Natural -> Maybe Natural
invert n x =
    if n == 1 then Just 0
    else if g == 1 then Just s
    else Nothing
  where
    (g,(s,_)) = Divides.egcd x n

divides :: Natural -> Natural -> Natural -> Bool
divides n a b = Divides.divides (gcd n b) (gcd n a)

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
import OpenTheory.Natural.Divides
import qualified OpenTheory.Natural.Bits as Bits

multiplyExponential :: (a -> a -> a) -> a -> a -> Natural -> a
multiplyExponential mult =
    multExp
  where
    multExp z x k =
        if k == 0 then z else multExp z' x' k'
      where
        z' = if Bits.headBits k then mult z x else z
        x' = mult x x
        k' = Bits.tailBits k

functionPower :: (a -> a) -> Natural -> a -> a
functionPower f =
    loop
  where
    loop n x =
       if n == 0 then x
       else let x' = f x in x' `seq` loop (n - 1) x'

normalize :: Natural -> Natural -> Natural
normalize n x = x `mod` n

add :: Natural -> Natural -> Natural -> Natural
add n x y = normalize n (x + y)

double :: Natural -> Natural -> Natural
double n x = add n x x

negate :: Natural -> Natural -> Natural
negate n x =
    if y == 0 then y else n - y
  where
    y = normalize n x

subtract :: Natural -> Natural -> Natural -> Natural
subtract n x y =
    if y <= x then normalize n (x - y)
    else Arithmetic.Modular.negate n (y - x)

multiply :: Natural -> Natural -> Natural -> Natural
multiply n x y = normalize n (x * y)

square :: Natural -> Natural -> Natural
square n x = multiply n x x

exp :: Natural -> Natural -> Natural -> Natural
exp n = multiplyExponential (multiply n) 1

exp2 :: Natural -> Natural -> Natural -> Natural
exp2 n x k = functionPower (square n) k x

invert :: Natural -> Natural -> Maybe Natural
invert n x =
    if g == 1 then Just s else Nothing
  where
    (g,(s,_)) = egcd x n

divide :: Natural -> Natural -> Natural -> Maybe Natural
divide n x y =
    case invert n y of
      Nothing -> Nothing
      Just z -> Just (multiply n x z)

{- |
module: Modexp
description: Modular arithmetic
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Modexp
where

import OpenTheory.Primitive.Natural
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

modneg :: Natural -> Natural -> Natural
modneg n x = let y = x `mod` n in if y == 0 then y else n - y

modadd :: Natural -> Natural -> Natural -> Natural
modadd n x y = (x + y) `mod` n

modsub :: Natural -> Natural -> Natural -> Natural
modsub n x y = if y <= x then (x - y) `mod` n else modneg n (y - x)

modmult :: Natural -> Natural -> Natural -> Natural
modmult n x y = (x * y) `mod` n

modsquare :: Natural -> Natural -> Natural
modsquare n x = modmult n x x

modexp :: Natural -> Natural -> Natural -> Natural
modexp n = multiplyExponential (modmult n) 1

modexp2 :: Natural -> Natural -> Natural -> Natural
modexp2 n x k = functionPower (modsquare n) k x

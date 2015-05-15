{- |
module: ModExp
description: Modular exponentiation using repeated-squaring
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module ModExp
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
    loop n x = if n == 0 then x else loop (n - 1) (f x)

modMult :: Natural -> Natural -> Natural -> Natural
modMult n x y = (x * y) `mod` n

modSquare :: Natural -> Natural -> Natural
modSquare n x = modMult n x x

modExp :: Natural -> Natural -> Natural -> Natural
modExp n = multiplyExponential (modMult n) 1

modDoubleExp :: Natural -> Natural -> Natural -> Natural
modDoubleExp n x k = functionPower (modSquare n) k x

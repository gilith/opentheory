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

modMult :: Natural -> Natural -> Natural -> Natural
modMult n x y = (x * y) `mod` n

modSquare :: Natural -> Natural -> Natural
modSquare n x = modMult n x x

modExp :: Natural -> Natural -> Natural -> Natural
modExp n x k = multiplyExponential (modMult n) 1 x k

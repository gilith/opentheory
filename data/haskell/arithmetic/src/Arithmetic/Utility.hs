{- |
module: Arithmetic.Utility
description: Utility functions
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.Utility
where

import OpenTheory.Primitive.Natural
import qualified OpenTheory.Natural.Bits as Bits

functionPower :: (a -> a) -> Natural -> a -> a
functionPower f =
    loop
  where
    loop n x =
       if n == 0 then x
       else let x' = f x in x' `seq` loop (n - 1) x'

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

factorTwos :: Natural -> (Natural,Natural)
factorTwos n =
   if Bits.headBits n then (0,n) else (r + 1, s)
  where
    (r,s) = factorTwos (Bits.tailBits n)

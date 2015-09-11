{- |
module: Arithmetic.Factor
description: Factorization algorithms
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.Factor
where

import OpenTheory.Primitive.Natural
import qualified OpenTheory.Natural.Bits as Bits
import qualified OpenTheory.Primitive.Random as Random
import qualified OpenTheory.Natural.Uniform as Uniform

import Arithmetic.Prime
import qualified Arithmetic.Lucas as Lucas
import qualified Arithmetic.Modular as Modular

williams :: Natural -> Random.Random -> Maybe Natural
williams n rnd =
    let g = gcd n a in if 1 < g then Just g else loop a primes
  where
    w = Bits.width n
    a = Uniform.random (n - 3) rnd + 2

    pow =
        Lucas.williamsNthExp two sub mult
      where
        two = Modular.normalize n 2
        sub = Modular.subtract n
        mult = Modular.multiply n

    loop v ps =
        if g == n then Nothing
        else if 1 < g then Just g
        else loop (pow v p k) (tail ps)
      where
        g = gcd n (v - 2)
        p = head ps
        -- log_p n = log_2 n / log_2 p <= |n| / (|p| - 1)
        k = w `div` (Bits.width p - 1)

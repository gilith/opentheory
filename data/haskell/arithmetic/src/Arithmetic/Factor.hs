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

williamsMethod :: Natural -> [Natural] -> [Natural] -> Maybe Natural
williamsMethod n =
    loop
  where
    w = Bits.width n

    loop [] _ = Nothing
    loop _ [] = Nothing
    loop vs (p : ps) =
        case fltr vs p k of
          Left g -> Just g
          Right vs' -> loop vs' ps
      where
        -- log_p n = log_2 n / log_2 p <= |n| / (|p| - 1)
        k = w `div` (Bits.width p - 1)

    fltr [] _ _ = Right []
    fltr (v : vs) p k =
        case check v p k of
          Left g -> Left g
          Right v' -> mcons v' (fltr vs p k)

    mcons (Just v) (Right vs) = Right (v : vs)
    mcons _ vs = vs

    check v p k =
        if g == n then Right Nothing
        else if 1 < g then Left g
        else Right (Just (pow v p k))
      where
        g = gcd n (v - 2)

    pow =
        Lucas.williamsNthExp two sub mult
      where
        two = Modular.normalize n 2
        sub = Modular.subtract n
        mult = Modular.multiply n

williamsBase :: Natural -> Natural -> Random.Random -> Either Natural [Natural]
williamsBase n =
    go
  where
    go x rnd =
        if x == 0 then Right []
        else mcons (gen r1) (go (x - 1) r2)
      where
        (r1,r2) = Random.split rnd

    mcons (Right v) (Right vs) = Right (v : vs)
    mcons _ vs = vs

    gen rnd =
        if 1 < g then Left g else Right v
      where
        v = Uniform.random (n - 3) rnd + 2
        g = gcd n v

-- Works for odd numbers at least 5
williams :: Natural -> Natural -> Maybe Natural -> Random.Random -> Maybe Natural
williams n x k rnd =
    case williamsBase n x rnd of
      Left g -> Just g
      Right vs -> williamsMethod n vs ps
  where
    ps = case k of
           Just m -> take (fromIntegral m) primes
           Nothing -> primes

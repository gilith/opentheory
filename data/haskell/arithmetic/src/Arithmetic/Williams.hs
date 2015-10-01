{- |
module: Arithmetic.Williams
description: Williams p+1 factorization method
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.Williams
where

--import Debug.Trace(trace)
import OpenTheory.Primitive.Natural
import qualified OpenTheory.Natural.Bits as Bits
import qualified OpenTheory.Primitive.Random as Random
import qualified OpenTheory.Natural.Uniform as Uniform

import Arithmetic.Prime
import Arithmetic.Utility
import qualified Arithmetic.Lucas as Lucas
import qualified Arithmetic.Modular as Modular

sequence :: a -> a -> (a -> a -> a) -> (a -> a -> a) -> a -> [a]
sequence one two sub mult p = Lucas.vSequence two sub mult p one

nthExp :: a -> (a -> a -> a) -> (a -> a -> a) -> a -> Natural -> Natural -> a
nthExp two sub mult p n k =
    if k == 0 then p
    else if n == 0 then two
    else functionPower nthSeq k p
  where
    l = init (Bits.toList n)
    sq z = sub (mult z z) two
    nthSeq v =
        w
      where
        (w,_) = foldr inc (v, sq v) l
        inc b (x,y) =
           if b then (z, sq y) else (sq x, z)
         where
           z = sub (mult x y) v

nth :: a -> (a -> a -> a) -> (a -> a -> a) -> a -> Natural -> a
nth two sub mult p n = nthExp two sub mult p n 1

base :: Natural -> Natural -> Random.Random -> Either Natural [Natural]
base n =
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

method :: Natural -> [Natural] -> [Natural] -> Maybe Natural
method n =
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
        else if 1 < g then
          --trace ("Williams p+1 method succeeded with prime " ++ show p) $
          Left g
        else Right (Just (pow v p k))
      where
        g = gcd n (v - 2)

    pow =
        nthExp two sub mult
      where
        two = Modular.normalize n 2
        sub = Modular.subtract n
        mult = Modular.multiply n

-- Works for odd numbers at least 5
factor :: Natural -> Maybe Natural ->
          Natural -> Random.Random -> Maybe Natural
factor x k n rnd =
    case base n x rnd of
      Left g -> Just g
      Right vs -> method n vs ps
  where
    ps = case k of
           Just m -> take (fromIntegral m) primes
           Nothing -> primes

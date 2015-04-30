{- |
module: GenuineSieve
description: A simple implementation of the genuine sieve of Eratosphenes
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module GenuineSieve
  ( primes )
where

import qualified Heap
import qualified Data.List as List
import OpenTheory.Primitive.Natural

newtype Sieve =
    Sieve { unSieve :: (Natural, Heap.Heap (Natural,Natural)) }

instance Show Sieve where
  show s = show (unSieve s)

perimeter :: Sieve -> Natural
perimeter (Sieve (n,_)) = n

initial :: Sieve
initial =
    Sieve (9, Heap.empty lep)
  where
    lep (p1,_) (p2,_) = p1 <= p2

add :: Natural -> Sieve -> Sieve
add p (Sieve (n,ps)) = Sieve (n, Heap.add (p * p, 2 * p) ps)

bump :: Sieve -> Sieve
bump (Sieve (_,ps)) =
    case Heap.remove ps of
      Nothing -> error "GenuineSieve.bump"
      Just ((kp,p),ps') -> Sieve (kp, Heap.add (kp + p, p) ps')

advance :: Natural -> Sieve -> [Natural]
advance m s =
    let n = perimeter s in
    if m < n
      then m : advance (m + 2) (add m s)
      else advance (if m == n then m + 2 else m) (bump s)

primes :: [Natural]
primes = 2 : advance 3 initial

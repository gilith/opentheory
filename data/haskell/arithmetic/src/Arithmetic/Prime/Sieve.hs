{- |
module: Arithmetic.Prime.Sieve
description: The genuine sieve of Eratosphenes
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.Prime.Sieve
where

import OpenTheory.Primitive.Natural

import qualified Arithmetic.Utility.Heap as Heap

newtype Sieve = Sieve { unSieve :: Heap.Heap (Natural,Natural) }

instance Show Sieve where
  show s = show (unSieve s)

initial :: Sieve
initial =
    Sieve (Heap.empty lep)
  where
    lep (kp1,_) (kp2,_) = kp1 <= kp2

-- let p = 2 * m + 1
-- 2m' + 1 = p * p = (2m + 1) * (2m + 1) = 2(((2m + 1) + 1) * m) + 1
-- Therefore, m' = ((2m + 1) + 1) * m = (p + 1) * m
add :: Natural -> Sieve -> (Natural,Sieve)
add m (Sieve ps) =
    (p, Sieve (Heap.add (m',p) ps))
  where
    p = 2 * m + 1
    m' = (p + 1) * m

bump :: Sieve -> (Natural,Sieve)
bump (Sieve ps) =
    case Heap.remove ps of
      Nothing -> error "GenuineSieve.bump"
      Just ((kp,p),ps') -> (kp, Sieve (Heap.add (kp + p, p) ps'))

advance :: Natural -> Natural -> Sieve -> [Natural]
advance m n s =
    if m < n
      then let (p,s') = add m s in p : advance m' n s'
      else let (n',s') = bump s in advance (if m == n then m' else m) n' s'
  where
    m' = m + 1

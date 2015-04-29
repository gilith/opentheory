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
perimeter s = fst (unSieve s)

initial :: Sieve
initial =
    Sieve (1, Heap.empty lep)
  where
    lep (p1,_) (p2,_) = p1 <= p2

next :: Sieve -> (Natural,Sieve)
next s =
    let (b, s') = increment s in
    if b then (perimeter s', s') else next s'

increment :: Sieve -> (Bool,Sieve)
increment =
    \s ->
      let (n,ps) = unSieve s in
      let n' = n + 2 in
      let (m,ps') = bump ps in
      if m <= n'
        then (False, Sieve (m,ps'))
        else (True, Sieve (n', Heap.add (n' * n', 2 * n') ps))
  where
    bump ps =
      case Heap.remove ps of
        Nothing -> (5,ps)
        Just ((kp,p),ps') -> (kp, Heap.add (kp + p, p) ps')

primes :: [Natural]
primes = 2 : List.unfoldr (Just . next) initial

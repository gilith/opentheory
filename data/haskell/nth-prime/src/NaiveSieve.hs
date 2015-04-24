{- |
module: NaiveSieve
description: A straightforward implementation of the naive sieve algorithm
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module NaiveSieve
  ( primes )
where

import qualified Data.List as List
import OpenTheory.Primitive.Natural

newtype Sieve =
  Sieve { unSieve :: (Natural,[(Natural,Natural)]) }

perimeter :: Sieve -> Natural
perimeter s = fst (unSieve s)

initial :: Sieve
initial = Sieve (1,[])

next :: Sieve -> (Natural,Sieve)
next s =
    let (b, s') = increment s in
    if b then (perimeter s', s') else next s'

increment :: Sieve -> (Bool,Sieve)
increment =
  \s ->
    let (n,ps) = unSieve s in
    let n' = n + 1 in
    let ps' = inc ps in
    let b = check ps' in
    let ps'' = if b then ps' ++ [(n',0)] else ps' in
    (b, Sieve (n', ps''))
  where
    inc = map (\(p,k) -> (p, (k + 1) `mod` p))

    check = List.all (\(_,k) -> k /= 0)

primes :: [Natural]
primes = List.unfoldr (Just . next) initial

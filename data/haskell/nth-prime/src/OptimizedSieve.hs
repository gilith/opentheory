{- |
module: $Header$
description: Prime natural numbers
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module OptimizedSieve
where

import qualified Data.List as List
import OpenTheory.Primitive.Natural

newtype Sieve =
  Sieve { unSieve :: (Natural,[(Natural,(Natural,Natural))]) }

initial :: Sieve
initial = Sieve (1,[])

increment :: Sieve -> (Bool,Sieve)
increment =
  \s ->
    let (n,ps) = unSieve s in
    let n' = n + 1 in
    let (b,ps') = inc n' 1 ps in
    (b, Sieve (n',ps'))
  where
  {-inc ::
        Natural -> Natural -> [(Natural,(Natural,Natural))] ->
          (Bool,[(Natural,(Natural,Natural))])-}
    inc n _ [] = (True, (n,(0,0)) : [])
    inc n i ((p,(k,j)) : ps) =
      let k' = (k + i) `mod` p in
      let j' = j + i in
      if k' == 0 then (False, (p,(0,j')) : ps)
      else let (b,ps') = inc n j' ps in (b, (p,(k',0)) : ps')

perimeter :: Sieve -> Natural
perimeter s = fst (unSieve s)

next :: Sieve -> (Natural,Sieve)
next s =
  let (b,s') = increment s in
  if b then (perimeter s', s') else next s'

primes :: [Natural]
primes = List.unfoldr (Just . next) initial

{- |
module: $Header$
description: Prime numbers
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Number.Natural.Prime.Sieve
where

import qualified OpenTheory.Primitive.Natural as Primitive.Natural

newtype Sieve =
  Sieve {
    unSieve ::
      (Primitive.Natural.Natural,
       [(Primitive.Natural.Natural,
         (Primitive.Natural.Natural, Primitive.Natural.Natural))])
  }

initial :: Sieve
initial = Sieve (1, [])

increment :: Sieve -> (Bool, Sieve)
increment =
  \s ->
    let (n, ps) = unSieve s in
    let n' = n + 1 in
    let (b, ps') = inc n' 1 ps in
    (b, Sieve (n', ps'))
  where
  {-inc ::
        Primitive.Natural.Natural -> Primitive.Natural.Natural ->
          [(Primitive.Natural.Natural,
            (Primitive.Natural.Natural, Primitive.Natural.Natural))] ->
          (Bool,
           [(Primitive.Natural.Natural,
             (Primitive.Natural.Natural, Primitive.Natural.Natural))])-}
    inc n _ [] = (True, (n, (0, 0)) : [])
    inc n i ((p, (k, j)) : ps) =
      let k' = (k + i) `mod` p in
      let j' = j + i in
      if k' == 0 then (False, (p, (0, j')) : ps)
      else let (b, ps') = inc n j' ps in (b, (p, (k', 0)) : ps')

perimeter :: Sieve -> Primitive.Natural.Natural
perimeter s = fst (unSieve s)

next :: Sieve -> (Primitive.Natural.Natural, Sieve)
next s =
  let (b, s') = increment s in if b then (perimeter s', s') else next s'

{- |
module: $Header$
description: Probability
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module OpenTheory.Random
where

import qualified OpenTheory.Primitive.Natural as Natural
import qualified OpenTheory.Primitive.Random as Random

vector :: (Random.Random -> a) -> Natural.Natural -> Random.Random -> [a]
vector f n r =
  if n == 0 then []
  else let (r1, r2) = Random.split r in f r1 : vector f (n - 1) r2

bits :: Natural.Natural -> Random.Random -> [Bool]
bits = vector Random.bit

{- |
module: $Header$
description: Probability
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module OpenTheory.List.Geometric
where

import qualified OpenTheory.Natural.Geometric as Geometric
import qualified OpenTheory.Primitive.Random as Primitive.Random
import qualified OpenTheory.Random as Random

random :: (Primitive.Random.Random -> a) -> Primitive.Random.Random -> [a]
random f r =
  let (r1, r2) = Primitive.Random.split r in
  Random.vector f (Geometric.random r1) r2

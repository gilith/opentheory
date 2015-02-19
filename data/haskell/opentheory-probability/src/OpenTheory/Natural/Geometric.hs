{- |
module: $Header$
description: Probability
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module OpenTheory.Natural.Geometric
where

import qualified OpenTheory.Primitive.Natural as Natural
import qualified OpenTheory.Primitive.Random as Random

fromRandom :: Random.Random -> Natural.Natural
fromRandom =
  loop 0
  where
  {-loop :: Natural.Natural -> Random.Random -> Natural.Natural-}
    loop n r =
      let (r1, r2) = Random.split r in
      if Random.bit r1 then n else loop (n + 1) r2

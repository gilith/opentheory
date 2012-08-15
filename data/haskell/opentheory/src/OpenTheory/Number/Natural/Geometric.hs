{- |
module: $Header$
description: The Haskell base
license: MIT

maintainer: Joe Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Number.Natural.Geometric
where

import qualified OpenTheory.Primitive.Natural as Primitive.Natural
import qualified OpenTheory.Primitive.Random as Primitive.Random

fromRandom ::
  Primitive.Random.Random ->
    (Primitive.Natural.Natural, Primitive.Random.Random)
fromRandom =
  \r -> let (r1, r2) = Primitive.Random.split r in (loop 0 r1, r2)
  where
  {-loop ::
        Primitive.Natural.Natural -> Primitive.Random.Random ->
          Primitive.Natural.Natural-}
    loop n r =
      let (b, r') = Primitive.Random.bit r in
      if b then n else loop (n + 1) r'

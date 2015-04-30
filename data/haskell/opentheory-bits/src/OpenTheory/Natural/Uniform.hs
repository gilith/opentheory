{- |
module: $Header$
description: Natural number to bit-list conversions
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module OpenTheory.Natural.Uniform
where

import qualified OpenTheory.Natural.Bits as Bits
import qualified OpenTheory.Primitive.Natural as Natural
import qualified OpenTheory.Primitive.Random as Primitive.Random
import qualified OpenTheory.Random as Random

random :: Natural.Natural -> Primitive.Random.Random -> Natural.Natural
random n =
  \r -> let w = Bits.width (n - 1) in loop w r
  where
  {-loop :: Natural.Natural -> Primitive.Random.Random -> Natural.Natural-}
    loop w r =
      let (r1, r2) = Primitive.Random.split r in
      let l = Random.bits w r1 in
      let m = Bits.fromList l in
      if m < n then m else loop w r2

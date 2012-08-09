{- |
Module: $Header$
Description: The Haskell base
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable
-}
module OpenTheory.Number.Natural.Uniform
where

import qualified OpenTheory.Number.Natural.Bits as Number.Natural.Bits
import qualified OpenTheory.Primitive.Natural as Primitive.Natural
import qualified OpenTheory.Primitive.Random as Primitive.Random
import qualified OpenTheory.Probability.Random as Probability.Random

fromRandom ::
  Primitive.Natural.Natural -> Primitive.Random.Random ->
    (Primitive.Natural.Natural, Primitive.Random.Random)
fromRandom n =
  \r ->
    let w = Number.Natural.Bits.width (n - 1) in
    let (r1, r2) = Primitive.Random.split r in
    (loop w r1 `mod` n, r2)
  where
  {-loop ::
        Primitive.Natural.Natural -> Primitive.Random.Random ->
          Primitive.Natural.Natural-}
    loop w r =
      let (l, r') = Probability.Random.bits w r in
      let m = Number.Natural.Bits.toNatural l in
      if m < n then m else loop w r'

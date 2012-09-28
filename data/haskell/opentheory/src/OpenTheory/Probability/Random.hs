{- |
module: $Header$
description: The Haskell base
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Probability.Random
where

import qualified OpenTheory.Data.List as Data.List
import qualified OpenTheory.Primitive.Natural as Primitive.Natural
import qualified OpenTheory.Primitive.Random as Primitive.Random

bits ::
  Primitive.Natural.Natural -> Primitive.Random.Random ->
    ([Bool], Primitive.Random.Random)
bits = Data.List.fromRandom Primitive.Random.bit

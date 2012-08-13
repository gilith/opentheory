{- |
Module: $Header$
Description: The Haskell base
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable
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

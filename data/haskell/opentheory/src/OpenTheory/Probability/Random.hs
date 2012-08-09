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

import qualified OpenTheory.Primitive.Natural as Primitive.Natural
import qualified OpenTheory.Primitive.Random as Primitive.Random

bits ::
  Primitive.Natural.Natural -> Primitive.Random.Random ->
    ([Bool], Primitive.Random.Random)
bits n r =
  if n == 0 then ([], r)
  else
    let (b, r') = Primitive.Random.bit r in
    let (l, r'') = bits (n - 1) r' in
    (b : l, r'')

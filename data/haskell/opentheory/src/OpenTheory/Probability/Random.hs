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

import qualified OpenTheory.Primitive.Number.Natural
import qualified OpenTheory.Primitive.Probability.Random

bits ::
  OpenTheory.Primitive.Number.Natural.Natural ->
    OpenTheory.Primitive.Probability.Random.Random ->
    ([Bool], OpenTheory.Primitive.Probability.Random.Random)
bits n r =
  if n == 0 then ([], r)
  else
    let (b, r') = OpenTheory.Primitive.Probability.Random.bit r in
    let (l, r'') = bits (n - 1) r' in
    (b : l, r'')

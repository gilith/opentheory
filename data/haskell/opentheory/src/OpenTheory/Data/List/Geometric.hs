{- |
module: $Header$
description: The Haskell base
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Data.List.Geometric
where

import qualified OpenTheory.Data.List as Data.List
import qualified OpenTheory.Number.Natural.Geometric
  as Number.Natural.Geometric
import qualified OpenTheory.Primitive.Random as Primitive.Random

fromRandom ::
  (Primitive.Random.Random -> (a, Primitive.Random.Random)) ->
    Primitive.Random.Random -> ([a], Primitive.Random.Random)
fromRandom d r =
  let (n, r') = Number.Natural.Geometric.fromRandom r in
  Data.List.fromRandom d n r'

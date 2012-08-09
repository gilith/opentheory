{- |
Module: $Header$
Description: The Haskell base
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable
-}
module OpenTheory.Number.Natural
where

import qualified OpenTheory.Primitive.Natural as Primitive.Natural
import qualified OpenTheory.Primitive.Random as Primitive.Random

fromRandom ::
  Primitive.Random.Random ->
    (Primitive.Natural.Natural, Primitive.Random.Random)
fromRandom =
  \r ->
    let (r1, r2) = Primitive.Random.split r in
    (dest False 0 1 0 r1 - 1, r2)
  where
  {-dest ::
        Bool -> Primitive.Natural.Natural -> Primitive.Natural.Natural ->
          Primitive.Natural.Natural -> Primitive.Random.Random ->
          Primitive.Natural.Natural-}
    dest b n f p r =
      let (b', r') = Primitive.Random.bit r in
      if b' && b then n
      else let s = f + p in dest b' (if b' then s + n else n) s f r'

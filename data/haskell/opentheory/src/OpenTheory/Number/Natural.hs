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

import qualified OpenTheory.Primitive.Number.Natural
import qualified OpenTheory.Primitive.Probability.Random

fromRandom ::
  OpenTheory.Primitive.Probability.Random.Random ->
    (OpenTheory.Primitive.Number.Natural.Natural,
     OpenTheory.Primitive.Probability.Random.Random)
fromRandom =
  \r ->
    let (r1, r2) = OpenTheory.Primitive.Probability.Random.split r in
    (dest False 0 1 0 r1 - 1, r2)
  where
  {-dest ::
        Bool -> OpenTheory.Primitive.Number.Natural.Natural ->
          OpenTheory.Primitive.Number.Natural.Natural ->
          OpenTheory.Primitive.Number.Natural.Natural ->
          OpenTheory.Primitive.Probability.Random.Random ->
          OpenTheory.Primitive.Number.Natural.Natural-}
    dest b n f p r =
      let (b', r') = OpenTheory.Primitive.Probability.Random.bit r in
      if b' && b then n
      else let s = f + p in dest b' (if b' then s + n else n) s f r'

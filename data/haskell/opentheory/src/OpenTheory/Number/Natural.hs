{- |
module: $Header$
description: The Haskell base
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Number.Natural
where

import qualified OpenTheory.Primitive.Natural as Primitive.Natural
import qualified OpenTheory.Primitive.Random as Primitive.Random

divides :: Primitive.Natural.Natural -> Primitive.Natural.Natural -> Bool
divides m n = if m == 0 then n == 0 else n `mod` m == 0

fromBool :: Bool -> Primitive.Natural.Natural
fromBool b = if b then 1 else 0

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

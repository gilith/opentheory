{- |
Module: $Header$
Description: The Haskell base
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable
-}
module OpenTheory.Data.List
where

import qualified OpenTheory.Primitive.Number.Natural
import qualified OpenTheory.Primitive.Probability.Random

equal :: (a -> a -> Bool) -> [a] -> [a] -> Bool
equal _ [] [] = True
equal _ [] (_ : _) = False
equal _ (_ : _) [] = False
equal eq (h1 : t1) (h2 : t2) = eq h1 h2 && equal eq t1 t2

fromRandom ::
  (OpenTheory.Primitive.Probability.Random.Random ->
     (a, OpenTheory.Primitive.Probability.Random.Random)) ->
    OpenTheory.Primitive.Probability.Random.Random ->
    ([a], OpenTheory.Primitive.Probability.Random.Random)
fromRandom d =
  \r ->
    let (r1, r2) = OpenTheory.Primitive.Probability.Random.split r in
    (dest [] r1, r2)
  where
  {-dest :: [a] -> OpenTheory.Primitive.Probability.Random.Random -> [a]-}
    dest l r =
      let (b, r') = OpenTheory.Primitive.Probability.Random.bit r in
      if b then l else let (x, r'') = d r' in dest (x : l) r''

size :: [a] -> OpenTheory.Primitive.Number.Natural.Natural
size [] = 0
size (_ : t) = 1 + size t

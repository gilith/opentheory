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

import qualified OpenTheory.Number.Natural.Geometric
  as Number.Natural.Geometric
import qualified OpenTheory.Primitive.Natural as Primitive.Natural
import qualified OpenTheory.Primitive.Random as Primitive.Random

fromRandom ::
  (Primitive.Random.Random -> (a, Primitive.Random.Random)) ->
    Primitive.Natural.Natural -> Primitive.Random.Random ->
    ([a], Primitive.Random.Random)
fromRandom d n r =
  if n == 0 then ([], r)
  else
    let (h, r') = d r in
    let (t, r'') = fromRandom d (n - 1) r' in
    (h : t, r'')

equal :: (a -> a -> Bool) -> [a] -> [a] -> Bool
equal _ [] [] = True
equal _ [] (_ : _) = False
equal _ (_ : _) [] = False
equal eq (h1 : t1) (h2 : t2) = eq h1 h2 && equal eq t1 t2

fromGeometricRandom ::
  (Primitive.Random.Random -> (a, Primitive.Random.Random)) ->
    Primitive.Random.Random -> ([a], Primitive.Random.Random)
fromGeometricRandom d r =
  let (n, r') = Number.Natural.Geometric.fromRandom r in fromRandom d n r'

size :: [a] -> Primitive.Natural.Natural
size [] = 0
size (_ : t) = 1 + size t

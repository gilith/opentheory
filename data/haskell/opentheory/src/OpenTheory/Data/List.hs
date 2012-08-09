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

import qualified OpenTheory.Primitive.Natural as Primitive.Natural
import qualified OpenTheory.Primitive.Random as Primitive.Random

equal :: (a -> a -> Bool) -> [a] -> [a] -> Bool
equal _ [] [] = True
equal _ [] (_ : _) = False
equal _ (_ : _) [] = False
equal eq (h1 : t1) (h2 : t2) = eq h1 h2 && equal eq t1 t2

fromRandom ::
  (Primitive.Random.Random -> (a, Primitive.Random.Random)) ->
    Primitive.Random.Random -> ([a], Primitive.Random.Random)
fromRandom d =
  \r -> let (r1, r2) = Primitive.Random.split r in (dest [] r1, r2)
  where
  {-dest :: [a] -> Primitive.Random.Random -> [a]-}
    dest l r =
      let (b, r') = Primitive.Random.bit r in
      if b then l else let (x, r'') = d r' in dest (x : l) r''

size :: [a] -> Primitive.Natural.Natural
size [] = 0
size (_ : t) = 1 + size t

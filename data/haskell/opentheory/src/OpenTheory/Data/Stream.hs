{- |
module: $Header$
description: The Haskell base
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Data.Stream
where

import qualified OpenTheory.Primitive.Natural as Primitive.Natural

nth :: [a] -> Primitive.Natural.Natural -> a
nth s n = if n == 0 then head s else nth (tail s) (n - 1)

take' :: [a] -> Primitive.Natural.Natural -> [a]
take' s n = if n == 0 then [] else head s : take' (tail s) (n - 1)

unfold :: (b -> (a, b)) -> b -> [a]
unfold f b = let (a, b') = f b in a : unfold f b'

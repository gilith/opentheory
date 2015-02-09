{- |
module: $Header$
description: Stream types
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module OpenTheory.Stream
where

import qualified OpenTheory.Primitive.Natural as Natural

nth :: [a] -> Natural.Natural -> a
nth s n = if n == 0 then head s else nth (tail s) (n - 1)

take' :: [a] -> Natural.Natural -> [a]
take' s n = if n == 0 then [] else head s : take' (tail s) (n - 1)

unfold :: (b -> (a, b)) -> b -> [a]
unfold f b = let (a, b') = f b in a : unfold f b'

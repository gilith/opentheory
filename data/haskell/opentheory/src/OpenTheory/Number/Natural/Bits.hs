{- |
module: $Header$
description: The Haskell base
license: MIT

maintainer: Joe Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Number.Natural.Bits
where

import qualified OpenTheory.Primitive.Natural as Primitive.Natural

toNatural :: [Bool] -> Primitive.Natural.Natural
toNatural [] = 0
toNatural (h : t) = 2 * toNatural t + if h then 1 else 0

width :: Primitive.Natural.Natural -> Primitive.Natural.Natural
width n = if n == 0 then 0 else width (n `div` 2) + 1

{- |
module: $Header$
description: The Haskell base
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Number.Natural.Bits
where

import qualified OpenTheory.Number.Natural as Number.Natural
import qualified OpenTheory.Primitive.Natural as Primitive.Natural

cons :: Bool -> Primitive.Natural.Natural -> Primitive.Natural.Natural
cons h t = Number.Natural.fromBool h + 2 * t

tail :: Primitive.Natural.Natural -> Primitive.Natural.Natural
tail n = n `div` 2

toNatural :: [Bool] -> Primitive.Natural.Natural
toNatural [] = 0
toNatural (h : t) = cons h (toNatural t)

width :: Primitive.Natural.Natural -> Primitive.Natural.Natural
width n = if n == 0 then 0 else width (tail n) + 1

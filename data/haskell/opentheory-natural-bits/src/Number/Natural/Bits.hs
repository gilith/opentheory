{- |
module: $Header$
description: Natural number to bit-list conversions
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module Number.Natural.Bits
where

import qualified OpenTheory.Natural as Natural
import qualified OpenTheory.Primitive.Natural as Primitive.Natural

head :: Primitive.Natural.Natural -> Bool
head n = Natural.odd n

tail :: Primitive.Natural.Natural -> Primitive.Natural.Natural
tail n = n `div` 2

width :: Primitive.Natural.Natural -> Primitive.Natural.Natural
width n = if n == 0 then 0 else width (tail n) + 1

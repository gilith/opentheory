{- |
module: $Header$
description: Natural number to bit-list conversions
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module OpenTheory.Natural.Bits
where

import qualified OpenTheory.Natural as Natural
import qualified OpenTheory.Primitive.Natural as Primitive.Natural

headBits :: Primitive.Natural.Natural -> Bool
headBits n = Natural.odd n

tailBits :: Primitive.Natural.Natural -> Primitive.Natural.Natural
tailBits n = n `div` 2

width :: Primitive.Natural.Natural -> Primitive.Natural.Natural
width n = if n == 0 then 0 else width (tailBits n) + 1

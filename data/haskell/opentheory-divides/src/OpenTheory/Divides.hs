{- |
module: $Header$
description: The divides relation on natural numbers
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module OpenTheory.Divides
where

import qualified OpenTheory.Primitive.Natural as Natural

divides :: Natural.Natural -> Natural.Natural -> Bool
divides m n = if m == 0 then n == 0 else n `mod` m == 0

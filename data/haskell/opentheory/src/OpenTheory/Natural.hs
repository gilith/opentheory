{- |
module: $Header$
description: The standard theory library
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module OpenTheory.Natural
where

import qualified OpenTheory.Primitive.Natural as Natural

odd :: Natural.Natural -> Bool
odd n = n `mod` 2 == 1

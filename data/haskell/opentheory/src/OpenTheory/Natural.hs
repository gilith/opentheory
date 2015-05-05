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

naturalEven :: Natural.Natural -> Bool
naturalEven n = n `mod` 2 == 0

naturalOdd :: Natural.Natural -> Bool
naturalOdd n = not (naturalEven n)

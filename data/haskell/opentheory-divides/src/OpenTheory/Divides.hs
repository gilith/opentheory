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
divides a b = if a == 0 then b == 0 else b `mod` a == 0

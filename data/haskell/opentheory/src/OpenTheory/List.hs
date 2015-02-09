{- |
module: $Header$
description: The standard theory library
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module OpenTheory.List
where

import qualified OpenTheory.Primitive.Natural as Natural

naturalLength :: [a] -> Natural.Natural
naturalLength [] = 0
naturalLength (_ : t) = naturalLength t + 1

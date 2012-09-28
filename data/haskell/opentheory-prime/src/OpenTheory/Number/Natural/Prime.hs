{- |
module: $Header$
description: Prime numbers
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Number.Natural.Prime
where

import qualified OpenTheory.Data.Stream as Data.Stream
import qualified OpenTheory.Number.Natural.Prime.Sieve as Sieve
import qualified OpenTheory.Primitive.Natural as Primitive.Natural

all :: [Primitive.Natural.Natural]
all = Data.Stream.unfold Sieve.next Sieve.initial

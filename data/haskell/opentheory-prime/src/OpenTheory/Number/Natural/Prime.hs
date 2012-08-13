{- |
Module: $Header$
Description: Prime numbers
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable
-}
module OpenTheory.Number.Natural.Prime
where

import qualified OpenTheory.Data.Stream as Data.Stream
import qualified OpenTheory.Number.Natural.Prime.Sieve as Sieve
import qualified OpenTheory.Primitive.Natural as Primitive.Natural

all :: [Primitive.Natural.Natural]
all = Data.Stream.unfold Sieve.next Sieve.initial

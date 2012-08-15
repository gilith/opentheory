{- |
module: $Header$
description: The Haskell base
license: MIT

maintainer: Joe Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Data.Byte
where

import qualified OpenTheory.Data.Byte.Bits as Bits
import qualified OpenTheory.Primitive.Byte as Primitive.Byte
import qualified OpenTheory.Primitive.Random as Primitive.Random
import qualified OpenTheory.Probability.Random as Probability.Random

fromRandom ::
  Primitive.Random.Random -> (Primitive.Byte.Byte, Primitive.Random.Random)
fromRandom r =
  let (r1, r2) = Primitive.Random.split r in
  let (l, _) = Probability.Random.bits 8 r1 in
  (Bits.toByte l, r2)

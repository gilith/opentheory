{- |
module: $Header$
description: The Haskell base
license: MIT

maintainer: Joe Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Data.Word16
where

import qualified OpenTheory.Data.Word16.Bits as Bits
import qualified OpenTheory.Primitive.Random as Primitive.Random
import qualified OpenTheory.Primitive.Word16 as Primitive.Word16
import qualified OpenTheory.Probability.Random as Probability.Random

fromRandom ::
  Primitive.Random.Random ->
    (Primitive.Word16.Word16, Primitive.Random.Random)
fromRandom r =
  let (r1, r2) = Primitive.Random.split r in
  let (l, _) = Probability.Random.bits 16 r1 in
  (Bits.toWord16 l, r2)

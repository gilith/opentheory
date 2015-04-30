{- |
module: $Header$
description: Bytes
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module OpenTheory.Byte
where

import qualified OpenTheory.Natural.Uniform as Uniform
import qualified OpenTheory.Primitive.Byte as Byte
import qualified OpenTheory.Primitive.Natural as Natural
import qualified OpenTheory.Primitive.Random as Random

width :: Natural.Natural
width = 8

modulus :: Natural.Natural
modulus = 2 ^ width

random :: Random.Random -> Byte.Byte
random r = Byte.fromNatural (Uniform.random modulus r)

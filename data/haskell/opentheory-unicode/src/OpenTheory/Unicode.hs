{- |
module: $Header$
description: Unicode characters
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module OpenTheory.Unicode
where

import qualified OpenTheory.Natural.Bits as Bits
import qualified OpenTheory.Natural.Uniform as Uniform
import qualified OpenTheory.Primitive.Natural as Natural
import qualified OpenTheory.Primitive.Random as Random

newtype Unicode = Unicode { unUnicode :: Natural.Natural }

destPlane :: Natural.Natural -> Natural.Natural
destPlane n = Bits.shiftRight n 16

destPosition :: Natural.Natural -> Natural.Natural
destPosition n = Bits.bound n 16

fromRandom :: Random.Random -> Unicode
fromRandom r =
  let n0 = Uniform.fromRandom 1111998 r in
  let n1 = if n0 < 55296 then n0 else n0 + 2048 in
  let pl = n1 `div` 65534 in
  let pos = n1 `mod` 65534 in
  let n2 = pos + Bits.shiftLeft pl 16 in
  Unicode n2

invariant :: Natural.Natural -> Bool
invariant n =
  let pl = destPlane n in
  let pos = destPosition n in
  pos < 65534 &&
  if not (pl == 0) then pl < 17
  else
    not (55296 <= pos && pos < 57344) && not (64976 <= pos && pos < 65008)

plane :: Unicode -> Natural.Natural
plane = destPlane . unUnicode

position :: Unicode -> Natural.Natural
position = destPosition . unUnicode

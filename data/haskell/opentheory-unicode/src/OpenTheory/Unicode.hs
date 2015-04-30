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
import qualified Test.QuickCheck as QuickCheck

newtype Unicode = Unicode { unUnicode :: Natural.Natural }
  deriving (Eq, Ord, Show)

destPlane :: Natural.Natural -> Natural.Natural
destPlane n = Bits.shiftRight n 16

destPosition :: Natural.Natural -> Natural.Natural
destPosition n = Bits.bound n 16

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

random :: Random.Random -> Unicode
random r =
  let n0 = Uniform.random 1111998 r in
  let n1 = if n0 < 55296 then n0 else n0 + 2048 in
  let n2 = if n1 < 64976 then n1 else n1 + 32 in
  let pl = n2 `div` 65534 in
  let pos = n2 `mod` 65534 in
  let n = pos + Bits.shiftLeft pl 16 in
  Unicode n

instance  QuickCheck.Arbitrary Unicode where
  arbitrary = fmap random QuickCheck.arbitrary

{- |
module: $Header$
description: Natural number to bit-list conversions
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module OpenTheory.Natural.Bits
where

import qualified OpenTheory.Natural as Natural
import qualified OpenTheory.Primitive.Natural as Primitive.Natural

singleton :: Bool -> Primitive.Natural.Natural
singleton b = if b then 1 else 0

cons :: Bool -> Primitive.Natural.Natural -> Primitive.Natural.Natural
cons h t = singleton h + 2 * t

append :: [Bool] -> Primitive.Natural.Natural -> Primitive.Natural.Natural
append l n = foldr cons n l

headBits :: Primitive.Natural.Natural -> Bool
headBits n = Natural.naturalOdd n

bit :: Primitive.Natural.Natural -> Primitive.Natural.Natural -> Bool
bit n i = headBits (Primitive.Natural.shiftRight n i)

bound ::
  Primitive.Natural.Natural -> Primitive.Natural.Natural ->
    Primitive.Natural.Natural
bound n k =
  n - Primitive.Natural.shiftLeft (Primitive.Natural.shiftRight n k) k

fromList :: [Bool] -> Primitive.Natural.Natural
fromList l = append l 0

tailBits :: Primitive.Natural.Natural -> Primitive.Natural.Natural
tailBits n = Primitive.Natural.shiftRight n 1

toList :: Primitive.Natural.Natural -> [Bool]
toList n = if n == 0 then [] else headBits n : toList (tailBits n)

toVector ::
  Primitive.Natural.Natural -> Primitive.Natural.Natural -> [Bool]
toVector n k =
  if k == 0 then [] else headBits n : toVector (tailBits n) (k - 1)

width :: Primitive.Natural.Natural -> Primitive.Natural.Natural
width n = if n == 0 then 0 else width (tailBits n) + 1

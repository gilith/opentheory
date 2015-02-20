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

import qualified Data.List as List
import qualified Number.Natural as Natural
import qualified OpenTheory.Natural
import qualified OpenTheory.Primitive.Natural as Primitive.Natural

singleton :: Bool -> Primitive.Natural.Natural
singleton b = if b then 1 else 0

cons :: Bool -> Primitive.Natural.Natural -> Primitive.Natural.Natural
cons h t = singleton h + 2 * t

append :: [Bool] -> Primitive.Natural.Natural -> Primitive.Natural.Natural
append l n = List.foldr cons n l

headBits :: Primitive.Natural.Natural -> Bool
headBits n = OpenTheory.Natural.odd n

shiftRight ::
  Primitive.Natural.Natural -> Primitive.Natural.Natural ->
    Primitive.Natural.Natural
shiftRight n k = n `div` (Natural.^) 2 k

bit :: Primitive.Natural.Natural -> Primitive.Natural.Natural -> Bool
bit n i = headBits (shiftRight n i)

bound ::
  Primitive.Natural.Natural -> Primitive.Natural.Natural ->
    Primitive.Natural.Natural
bound n k = n `mod` (Natural.^) 2 k

fromList :: [Bool] -> Primitive.Natural.Natural
fromList l = append l 0

shiftLeft ::
  Primitive.Natural.Natural -> Primitive.Natural.Natural ->
    Primitive.Natural.Natural
shiftLeft n k = (Natural.^) 2 k * n

tailBits :: Primitive.Natural.Natural -> Primitive.Natural.Natural
tailBits n = n `div` 2

toList :: Primitive.Natural.Natural -> [Bool]
toList n = if n == 0 then [] else headBits n : toList (tailBits n)

width :: Primitive.Natural.Natural -> Primitive.Natural.Natural
width n = if n == 0 then 0 else width (tailBits n) + 1

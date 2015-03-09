{- |
module: $Header$
description: Primitive byte functions
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Primitive.Byte
  ( Byte,
    and,
    bit,
    fromNatural,
    not,
    or,
    shiftLeft,
    shiftRight,
    toNatural )
where

import Prelude (Bool, (<), (&&), fromIntegral)
import qualified Data.Bits
import qualified Data.Word
import qualified OpenTheory.Primitive.Natural as Natural

type Byte = Data.Word.Word8

and :: Byte -> Byte -> Byte
and w1 w2 = (Data.Bits..&.) w1 w2

bit :: Byte -> Natural.Natural -> Bool
bit w i = i < 8 && Data.Bits.testBit w (fromIntegral i)

fromNatural :: Natural.Natural -> Byte
fromNatural = fromIntegral

not :: Byte -> Byte
not w = (Data.Bits.complement) w

or :: Byte -> Byte -> Byte
or w1 w2 = (Data.Bits..|.) w1 w2

shiftLeft :: Byte -> Natural.Natural -> Byte
shiftLeft w n =
    if n < 8 then (Data.Bits.shiftL) w (fromIntegral n) else 0

shiftRight :: Byte -> Natural.Natural -> Byte
shiftRight w n =
    if n < 8 then (Data.Bits.shiftR) w (fromIntegral n) else 0

toNatural :: Byte -> Natural.Natural
toNatural = fromIntegral

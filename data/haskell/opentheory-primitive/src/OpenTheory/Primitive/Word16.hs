{- |
module: $Header$
description: Primitive 16-bit word functions
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Primitive.Word16
  ( Word16,
    and,
    bit,
    fromBytes,
    fromNatural,
    not,
    or,
    shiftLeft,
    shiftRight,
    toBytes )
where

import Prelude (Bool, (<), (&&), fromIntegral)
import qualified Data.Bits
import qualified Data.Word
import qualified OpenTheory.Primitive.Byte as Primitive.Byte
import qualified OpenTheory.Primitive.Natural as Primitive.Natural

type Word16 = Data.Word.Word16

and :: Word16 -> Word16 -> Word16
and w1 w2 = (Data.Bits..&.) w1 w2

bit :: Word16 -> Primitive.Natural.Natural -> Bool
bit w i = i < 16 && Data.Bits.testBit w (fromIntegral i)

fromBytes :: Primitive.Byte.Byte -> Primitive.Byte.Byte -> Word16
fromBytes b0 b1 = or (fromIntegral b0) (shiftLeft (fromIntegral b1) 8)

fromNatural :: Primitive.Natural.Natural -> Word16
fromNatural = fromIntegral

not :: Word16 -> Word16
not w = (Data.Bits.complement) w

or :: Word16 -> Word16 -> Word16
or w1 w2 = (Data.Bits..|.) w1 w2

shiftLeft :: Word16 -> Primitive.Natural.Natural -> Word16
shiftLeft w n =
    if n < 16 then (Data.Bits.shiftL) w (fromIntegral n) else 0

shiftRight :: Word16 -> Primitive.Natural.Natural -> Word16
shiftRight w n =
    if n < 16 then (Data.Bits.shiftR) w (fromIntegral n) else 0

toBytes :: Word16 -> (Primitive.Byte.Byte,Primitive.Byte.Byte)
toBytes w = (fromIntegral w, fromIntegral (shiftRight w 8))

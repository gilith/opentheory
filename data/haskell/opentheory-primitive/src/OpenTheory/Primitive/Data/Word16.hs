{- |
Module: $Header$
Description: Word16 primitive functions
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable

Word16 primitive functions
-}
module OpenTheory.Primitive.Data.Word16
  ( and,
    bit,
    fromBytes,
    fromNatural,
    or,
    shiftLeft,
    shiftRight,
    toBytes )
where

import Prelude (Bool, (<), (&&), fromIntegral)
import qualified Data.Bits
import qualified Data.Word
import qualified OpenTheory.Primitive.Number.Natural

and :: Data.Word.Word16 -> Data.Word.Word16 -> Data.Word.Word16
and w1 w2 = (Data.Bits..&.) w1 w2

bit :: Data.Word.Word16 -> OpenTheory.Primitive.Number.Natural.Natural -> Bool
bit w i = i < 16 && Data.Bits.testBit w (fromIntegral i)

fromBytes :: Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word16
fromBytes b0 b1 = or (fromIntegral b0) (shiftLeft (fromIntegral b1) 8)

fromNatural :: OpenTheory.Primitive.Number.Natural.Natural -> Data.Word.Word16
fromNatural = fromIntegral

or :: Data.Word.Word16 -> Data.Word.Word16 -> Data.Word.Word16
or w1 w2 = (Data.Bits..|.) w1 w2

shiftLeft ::
    Data.Word.Word16 -> OpenTheory.Primitive.Number.Natural.Natural ->
    Data.Word.Word16
shiftLeft w n =
    if n < 16 then (Data.Bits.shiftL) w (fromIntegral n) else 0

shiftRight ::
    Data.Word.Word16 -> OpenTheory.Primitive.Number.Natural.Natural ->
    Data.Word.Word16
shiftRight w n =
    if n < 16 then (Data.Bits.shiftR) w (fromIntegral n) else 0

toBytes :: Data.Word.Word16 -> (Data.Word.Word8,Data.Word.Word8)
toBytes w = (fromIntegral w, fromIntegral (shiftRight w 8))

{- |
Module: $Header$
Description: Byte primitive functions
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable

Byte primitive functions
-}
module OpenTheory.Primitive.Data.Byte
  ( Byte(..),
    and,
    bit,
    fromNatural,
    or,
    shiftLeft,
    shiftRight )
where

import Prelude (Bool, (<), (&&), fromIntegral)
import qualified Data.Bits
import qualified Data.Word
import qualified OpenTheory.Primitive.Number.Natural

type Byte = Data.Word.Word8

and :: Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8
and w1 w2 = (Data.Bits..&.) w1 w2

bit :: Data.Word.Word8 -> OpenTheory.Primitive.Number.Natural.Natural -> Bool
bit w i = i < 8 && Data.Bits.testBit w (fromIntegral i)

fromNatural :: OpenTheory.Primitive.Number.Natural.Natural -> Data.Word.Word8
fromNatural = fromIntegral

or :: Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8
or w1 w2 = (Data.Bits..|.) w1 w2

shiftLeft ::
    Data.Word.Word8 -> OpenTheory.Primitive.Number.Natural.Natural ->
    Data.Word.Word8
shiftLeft w n =
    if n < 8 then (Data.Bits.shiftL) w (fromIntegral n) else 0

shiftRight ::
    Data.Word.Word8 -> OpenTheory.Primitive.Number.Natural.Natural ->
    Data.Word.Word8
shiftRight w n =
    if n < 8 then (Data.Bits.shiftR) w (fromIntegral n) else 0

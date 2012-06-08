{- |
Module: $Header$
Description: Word functions
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable

A natural number type
-}
module OpenTheory.Word
  ( word8ToWord16,
    word16ToWord8 )
where

import qualified Data.Bits
import qualified Data.Word

word8ToWord16 :: Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word16
word8ToWord16 b1 b2 =
    let w1 = fromIntegral b1 in
    let w2 = fromIntegral b2 in
    (Data.Bits..|.) (Data.Bits.shift w1 8) w2

word16ToWord8 :: Data.Word.Word16 -> (Data.Word.Word8,Data.Word.Word8)
word16ToWord8 w =
    let w1 = Data.Bits.shiftR ((Data.Bits..&.) w 65280) 8 in
    let w2 = (Data.Bits..&.) w 255 in
    (fromIntegral w1, fromIntegral w2)

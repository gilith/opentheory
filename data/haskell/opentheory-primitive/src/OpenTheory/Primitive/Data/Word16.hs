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
  ( fromBytes,
    toBytes )
where

import qualified Data.Bits
import qualified Data.Word

fromBytes :: Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word16
fromBytes b1 b2 =
    let w1 = fromIntegral b1 in
    let w2 = fromIntegral b2 in
    (Data.Bits..|.) (Data.Bits.shift w1 8) w2

toBytes :: Data.Word.Word16 -> (Data.Word.Word8,Data.Word.Word8)
toBytes w =
    let w1 = Data.Bits.shiftR ((Data.Bits..&.) w 65280) 8 in
    let w2 = (Data.Bits..&.) w 255 in
    (fromIntegral w1, fromIntegral w2)

{- |
Module: $Header$
Description: A verified UTF8 parser
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable

A verified UTF8 parser
-}
module OpenTheory.Char
  ( Plane(..),
    Position(..),
    Unicode(..),
    decodeStream,
    decode,
    encode)
where

import qualified Data.Bits
import qualified Data.Word
import qualified OpenTheory.Parser
import qualified OpenTheory.Word
import qualified Test.QuickCheck

newtype Plane =
    Plane { unPlane :: Data.Word.Word8 }
  deriving (Eq,Show)

instance Test.QuickCheck.Arbitrary Plane where
  arbitrary = fmap Plane
                (Test.QuickCheck.suchThat Test.QuickCheck.arbitrary predicate)
      where
    predicate i = i < 17

newtype Position =
    Position { unPosition :: Data.Word.Word16 }
  deriving (Eq,Show)

instance Test.QuickCheck.Arbitrary Position where
  arbitrary = fmap Position Test.QuickCheck.arbitrary

newtype Unicode =
    Unicode { unUnicode :: (Plane,Position) }
  deriving (Eq,Show)

instance Test.QuickCheck.Arbitrary Unicode where
  arbitrary = fmap Unicode
                (Test.QuickCheck.suchThat Test.QuickCheck.arbitrary predicate)
      where
    predicate (Plane pl, Position pos) =
        pl /= 0 || pos < 55296 || (57343 < pos && pos < 65534)

isContinuationByte :: Data.Word.Word8 -> Bool
isContinuationByte b = Data.Bits.testBit b 7 && not (Data.Bits.testBit b 6)

parseContinuationByte :: OpenTheory.Parser.Parser Data.Word.Word8 Data.Word.Word8
parseContinuationByte = OpenTheory.Parser.parseSome isContinuationByte

parseTwoContinuationBytes ::
    OpenTheory.Parser.Parser Data.Word.Word8 (Data.Word.Word8,Data.Word.Word8)
parseTwoContinuationBytes =
    OpenTheory.Parser.parsePair parseContinuationByte parseContinuationByte

parseThreeContinuationBytes ::
    OpenTheory.Parser.Parser Data.Word.Word8
      (Data.Word.Word8,(Data.Word.Word8,Data.Word.Word8))
parseThreeContinuationBytes =
    OpenTheory.Parser.parsePair parseContinuationByte parseTwoContinuationBytes

parser :: OpenTheory.Parser.Parser Data.Word.Word8 Unicode
parser =
    OpenTheory.Parser.Parser parse
  where
    parse b0 s =
      if Data.Bits.testBit b0 7
      then
        if Data.Bits.testBit b0 6
        then
          if Data.Bits.testBit b0 5
          then
            if Data.Bits.testBit b0 4
            then
              if Data.Bits.testBit b0 3
              then Nothing
              else
                case OpenTheory.Parser.parse parseThreeContinuationBytes s of
                  Nothing -> Nothing
                  Just ((b1,(b2,b3)),s') ->
                    let w = Data.Bits.shiftL ((Data.Bits..&.) b0 7) 2 in
                    let z = Data.Bits.shiftR ((Data.Bits..&.) b1 48) 4 in
                    let p = (Data.Bits..|.) w z in
                    if p == 0 || 16 < p
                    then Nothing
                    else
                      let pl = Plane p in
                      let z0 = Data.Bits.shiftL ((Data.Bits..&.) b1 15) 4 in
                      let y0 = Data.Bits.shiftR ((Data.Bits..&.) b2 60) 2 in
                      let p0 = (Data.Bits..|.) z0 y0 in
                      let y1 = Data.Bits.shiftL ((Data.Bits..&.) b2 3) 6 in
                      let x1 = (Data.Bits..&.) b3 63 in
                      let p1 = (Data.Bits..|.) y1 x1 in
                      let pos = Position (OpenTheory.Word.word8ToWord16 p0 p1) in
                      let ch = Unicode (pl,pos) in
                      Just (ch,s')
            else
              case OpenTheory.Parser.parse parseTwoContinuationBytes s of
                Nothing -> Nothing
                Just ((b1,b2),s') ->
                  let z0 = Data.Bits.shiftL ((Data.Bits..&.) b0 15) 4 in
                  let y0 = Data.Bits.shiftR ((Data.Bits..&.) b1 60) 2 in
                  let p0 = (Data.Bits..|.) z0 y0 in
                  if p0 < 8 || (216 <= p0 && p0 <= 223)
                  then Nothing
                  else
                    let y1 = Data.Bits.shiftL ((Data.Bits..&.) b1 3) 6 in
                    let x1 = (Data.Bits..&.) b2 63 in
                    let p1 = (Data.Bits..|.) y1 x1 in
                    if p0 == 255 && 254 <= p1
                    then Nothing
                    else
                      let pl = Plane 0 in
                      let pos = Position (OpenTheory.Word.word8ToWord16 p0 p1) in
                      let ch = Unicode (pl,pos) in
                      Just (ch,s')
          else
            case OpenTheory.Parser.parse parseContinuationByte s of
              Nothing -> Nothing
              Just (b1,s') ->
                let pl = Plane 0 in
                let p0 = Data.Bits.shiftR ((Data.Bits..&.) b0 28) 2 in
                let y1 = Data.Bits.shiftL ((Data.Bits..&.) b0 3) 6 in
                let x1 = (Data.Bits..&.) b1 63 in
                let p1 = (Data.Bits..|.) y1 x1 in
                if p0 == 0 && p1 < 128
                then Nothing
                else
                  let pos = Position (OpenTheory.Word.word8ToWord16 p0 p1) in
                  let ch = Unicode (pl,pos) in
                  Just (ch,s')
        else
          Nothing
      else
        let pl = Plane 0 in
        let pos = Position (OpenTheory.Word.word8ToWord16 0 b0) in
        let ch = Unicode (pl,pos) in
        Just (ch,s)

printer :: Unicode -> [Data.Word.Word8]
printer (Unicode (pl,pos)) =
    let p = unPlane pl in
    let (p0,p1) = OpenTheory.Word.word16ToWord8 (unPosition pos) in
    if p == 0
    then
      if p0 == 0
      then
        if Data.Bits.testBit p1 7
        then
          let b00 = Data.Bits.shiftR ((Data.Bits..&.) p1 192) 6 in
          let b0 = (Data.Bits..|.) 192 b00 in
          let b10 = (Data.Bits..&.) p1 63 in
          let b1 = (Data.Bits..|.) 128 b10 in
          [b0,b1]
        else
          let b0 = p1 in
          [b0]
      else
        if (Data.Bits..&.) 248 p0 == 0
        then
          let b00 = Data.Bits.shiftL p0 2 in
          let b01 = Data.Bits.shiftR ((Data.Bits..&.) p1 192) 6 in
          let b0 = (Data.Bits..|.) 192 ((Data.Bits..|.) b00 b01) in
          let b10 = (Data.Bits..&.) p1 63 in
          let b1 = (Data.Bits..|.) 128 b10 in
          [b0,b1]
        else
          let b00 = Data.Bits.shiftR ((Data.Bits..&.) p0 240) 4 in
          let b0 = (Data.Bits..|.) 224 b00 in
          let b10 = Data.Bits.shiftL ((Data.Bits..&.) p0 15) 2 in
          let b11 = Data.Bits.shiftR ((Data.Bits..&.) p1 192) 6 in
          let b1 = (Data.Bits..|.) 128 ((Data.Bits..|.) b10 b11) in
          let b20 = (Data.Bits..&.) p1 63 in
          let b2 = (Data.Bits..|.) 128 b20 in
          [b0,b1,b2]
    else
      let b00 = Data.Bits.shiftR ((Data.Bits..&.) p 28) 2 in
      let b0 = (Data.Bits..|.) 240 b00 in
      let b10 = Data.Bits.shiftL ((Data.Bits..&.) p 3) 4 in
      let b11 = Data.Bits.shiftR ((Data.Bits..&.) p0 240) 4 in
      let b1 = (Data.Bits..|.) 128 ((Data.Bits..|.) b10 b11) in
      let b20 = Data.Bits.shiftL ((Data.Bits..&.) p0 15) 2 in
      let b21 = Data.Bits.shiftR ((Data.Bits..&.) p1 192) 6 in
      let b2 = (Data.Bits..|.) 128 ((Data.Bits..|.) b20 b21) in
      let b30 = (Data.Bits..&.) p1 63 in
      let b3 = (Data.Bits..|.) 128 b30 in
      [b0,b1,b2,b3]

decodeStream :: OpenTheory.Parser.Stream Data.Word.Word8
             -> OpenTheory.Parser.Stream Unicode
decodeStream = OpenTheory.Parser.parseStream parser

decode :: [Data.Word.Word8] -> Maybe [Unicode]
decode = OpenTheory.Parser.toList . decodeStream . OpenTheory.Parser.fromList

encode :: [Unicode] -> [Data.Word.Word8]
encode = concat . map printer

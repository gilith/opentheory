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
import qualified OpenTheory.Natural
import qualified OpenTheory.Parser
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

data Unicode =
    Unicode Plane Position
  deriving (Eq,Show)

instance Test.QuickCheck.Arbitrary Unicode where
  arbitrary = fmap (\(pl,pos) -> Unicode pl pos)
                Test.QuickCheck.arbitrary

parser :: OpenTheory.Parser.Parser Data.Word.Word8 Unicode
parser =
    OpenTheory.Parser.Parser p
  where
    p b t =
        if Data.Bits.testBit b 7
          then
            Nothing
          else
            let pl = Plane 0 in
            let pos = Position (fromInteger (toInteger b)) in
            let ch = Unicode pl pos in
            Just (ch,t)

printer :: Unicode -> [Data.Word.Word8]
printer (Unicode pl pos) =
    let pli = unPlane pl in
    let posi = unPosition pos in
    if pli == 0
      then
        if posi < 128
          then [fromInteger (toInteger posi)]
          else []
      else
        []

decodeStream :: OpenTheory.Parser.Stream Data.Word.Word8
             -> OpenTheory.Parser.Stream Unicode
decodeStream = OpenTheory.Parser.parse parser

decode :: [Data.Word.Word8] -> Maybe [Unicode]
decode = OpenTheory.Parser.toList . decodeStream . OpenTheory.Parser.fromList

encode :: [Unicode] -> [Data.Word.Word8]
encode = concat . map printer

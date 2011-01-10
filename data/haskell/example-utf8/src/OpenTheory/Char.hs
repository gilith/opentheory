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
    decodeToken,
    decode,
    encode)
where

import Data.Bits
import Data.Word
import OpenTheory.Natural
import OpenTheory.Parser
import Test.QuickCheck

newtype Plane =
    Plane { unPlane :: OpenTheory.Natural.Natural }
  deriving (Eq,Show)

instance Arbitrary Plane where
  arbitrary = fmap Plane (Test.QuickCheck.suchThat arbitrary predicate)
      where
    predicate i = i < 17

newtype Position =
    Position { unPosition :: Data.Word.Word16 }
  deriving (Eq,Show)

instance Arbitrary Position where
  arbitrary = fmap Position arbitrary

data Unicode =
    Unicode Plane Position
  deriving (Eq,Show)

instance Arbitrary Unicode where
  arbitrary = fmap (\(pl,pos) -> Unicode pl pos) arbitrary

parser :: Parser Word8 Unicode
parser =
    Parser p
  where
    p b t =
        if testBit b 7
          then
            Nothing
          else
            let pl = Plane 0 in
            let pos = Position (fromInteger (toInteger b)) in
            let ch = Unicode pl pos in
            Just (ch,t)

printer :: Unicode -> [Word8]
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

decodeToken :: Token Word8 -> Token Unicode
decodeToken = OpenTheory.Parser.parse parser

decode :: [Word8] -> Maybe [Unicode]
decode = OpenTheory.Parser.toList . decodeToken . OpenTheory.Parser.fromList

encode :: [Unicode] -> [Word8]
encode = concat . map printer

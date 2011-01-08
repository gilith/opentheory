{- |
Module: $Header$
Description: A verified UTF8 parser
License: MIT
License-file: LICENSE

Maintainer: Joe Hurd
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
import OpenTheory.Parser

newtype Plane =
    Plane { unPlane :: Integer }
  deriving (Eq,Show)

newtype Position =
    Position { unPosition :: Data.Word.Word16 }
  deriving (Eq,Show)

data Unicode =
    Unicode Plane Position
  deriving (Eq,Show)

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

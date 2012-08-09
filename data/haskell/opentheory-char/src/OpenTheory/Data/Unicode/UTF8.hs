{- |
Module: $Header$
Description: Unicode characters
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable
-}
module OpenTheory.Data.Unicode.UTF8
where

import qualified OpenTheory.Data.Unicode as Data.Unicode
import qualified OpenTheory.Parser as Parser
import qualified OpenTheory.Primitive.Byte as Primitive.Byte
import qualified OpenTheory.Primitive.Word16 as Primitive.Word16

isContinuationByte :: Primitive.Byte.Byte -> Bool
isContinuationByte b =
  Primitive.Byte.bit b 7 && not (Primitive.Byte.bit b 6)

parseContinuationByte ::
  Parser.Parser Primitive.Byte.Byte Primitive.Byte.Byte
parseContinuationByte = Parser.parseSome isContinuationByte

parseTwoContinuationBytes ::
  Parser.Parser Primitive.Byte.Byte
    (Primitive.Byte.Byte, Primitive.Byte.Byte)
parseTwoContinuationBytes =
  Parser.parsePair parseContinuationByte parseContinuationByte

parseThreeContinuationBytes ::
  Parser.Parser Primitive.Byte.Byte
    (Primitive.Byte.Byte, (Primitive.Byte.Byte, Primitive.Byte.Byte))
parseThreeContinuationBytes =
  Parser.parsePair parseContinuationByte parseTwoContinuationBytes

decoder :: Parser.Parser Primitive.Byte.Byte Data.Unicode.Unicode
decoder =
  Parser.Parser parse
  where
  {-parse ::
        Primitive.Byte.Byte -> Parser.Stream.Stream Primitive.Byte.Byte ->
          Maybe
            (Data.Unicode.Unicode,
             Parser.Stream.Stream Primitive.Byte.Byte)-}
    parse b0 s =
      if Primitive.Byte.bit b0 7 then
        if Primitive.Byte.bit b0 6 then
          if Primitive.Byte.bit b0 5 then
            if Primitive.Byte.bit b0 4 then
              if Primitive.Byte.bit b0 3 then Nothing
              else
                Parser.parse
                  (Parser.partialMap (decode3 b0)
                     parseThreeContinuationBytes) s
            else
              Parser.parse
                (Parser.partialMap (decode2 b0) parseTwoContinuationBytes)
                s
          else
            Parser.parse
              (Parser.partialMap (decode1 b0) parseContinuationByte) s
        else Nothing
      else
        let pl = Data.Unicode.Plane 0 in
        let pos =
              Data.Unicode.Position (Primitive.Word16.fromBytes b0 0) in
        let ch = Data.Unicode.Unicode (pl, pos) in
        Just (ch, s)

  {-decode1 ::
        Primitive.Byte.Byte -> Primitive.Byte.Byte ->
          Maybe Data.Unicode.Unicode-}
    decode1 b0 b1 =
      let pl = Data.Unicode.Plane 0 in
      let p1 = Primitive.Byte.shiftRight (Primitive.Byte.and b0 28) 2 in
      let y0 = Primitive.Byte.shiftLeft (Primitive.Byte.and b0 3) 6 in
      let x0 = Primitive.Byte.and b1 63 in
      let p0 = Primitive.Byte.or y0 x0 in
      if p1 == 0 && not (Primitive.Byte.bit p0 7) then Nothing
      else
        let pos =
              Data.Unicode.Position (Primitive.Word16.fromBytes p0 p1) in
        let ch = Data.Unicode.Unicode (pl, pos) in
        Just ch

  {-decode2 ::
        Primitive.Byte.Byte ->
          (Primitive.Byte.Byte, Primitive.Byte.Byte) ->
          Maybe Data.Unicode.Unicode-}
    decode2 b0 (b1, b2) =
      let z1 = Primitive.Byte.shiftLeft (Primitive.Byte.and b0 15) 4 in
      let y1 = Primitive.Byte.shiftRight (Primitive.Byte.and b1 60) 2 in
      let p1 = Primitive.Byte.or z1 y1 in
      if p1 < 8 || 216 <= p1 && p1 <= 223 then Nothing
      else
        let y0 = Primitive.Byte.shiftLeft (Primitive.Byte.and b1 3) 6 in
        let x0 = Primitive.Byte.and b2 63 in
        let p0 = Primitive.Byte.or y0 x0 in
        if p1 == 255 && 254 <= p0 then Nothing
        else
          let pl = Data.Unicode.Plane 0 in
          let pos =
                Data.Unicode.Position (Primitive.Word16.fromBytes p0 p1) in
          let ch = Data.Unicode.Unicode (pl, pos) in
          Just ch

  {-decode3 ::
        Primitive.Byte.Byte ->
          (Primitive.Byte.Byte,
           (Primitive.Byte.Byte, Primitive.Byte.Byte)) ->
          Maybe Data.Unicode.Unicode-}
    decode3 b0 (b1, (b2, b3)) =
      let w = Primitive.Byte.shiftLeft (Primitive.Byte.and b0 7) 2 in
      let z = Primitive.Byte.shiftRight (Primitive.Byte.and b1 48) 4 in
      let p = Primitive.Byte.or w z in
      if p == 0 || 16 < p then Nothing
      else
        let pl = Data.Unicode.Plane p in
        let z1 = Primitive.Byte.shiftLeft (Primitive.Byte.and b1 15) 4 in
        let y1 = Primitive.Byte.shiftRight (Primitive.Byte.and b2 60) 2 in
        let p1 = Primitive.Byte.or z1 y1 in
        let y0 = Primitive.Byte.shiftLeft (Primitive.Byte.and b2 3) 6 in
        let x0 = Primitive.Byte.and b3 63 in
        let p0 = Primitive.Byte.or y0 x0 in
        let pos =
              Data.Unicode.Position (Primitive.Word16.fromBytes p0 p1) in
        let ch = Data.Unicode.Unicode (pl, pos) in
        Just ch

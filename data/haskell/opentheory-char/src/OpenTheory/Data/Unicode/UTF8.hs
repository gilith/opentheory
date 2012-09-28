{- |
module: $Header$
description: Unicode characters
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Data.Unicode.UTF8
where

import qualified OpenTheory.Data.Unicode as Data.Unicode
import qualified OpenTheory.Parser as Parser
import qualified OpenTheory.Parser.Stream as Parser.Stream
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

decodeStream ::
  Parser.Stream.Stream Primitive.Byte.Byte ->
    Parser.Stream.Stream Data.Unicode.Unicode
decodeStream = Parser.parseStream decoder

decode :: [Primitive.Byte.Byte] -> Maybe [Data.Unicode.Unicode]
decode bs = Parser.Stream.toList (decodeStream (Parser.Stream.fromList bs))

encoder :: Data.Unicode.Unicode -> [Primitive.Byte.Byte]
encoder =
  \ch ->
    let (pl, pos) = Data.Unicode.unUnicode ch in
    let p = Data.Unicode.unPlane pl in
    let (p0, p1) =
          Primitive.Word16.toBytes (Data.Unicode.unPosition pos) in
    if p == 0 then
      if p1 == 0 && not (Primitive.Byte.bit p0 7) then p0 : []
      else if Primitive.Byte.and 248 p1 == 0 then encode1 p1 p0
      else encode2 p1 p0
    else encode3 p p1 p0
  where
  {-encode1 ::
        Primitive.Byte.Byte -> Primitive.Byte.Byte ->
          [Primitive.Byte.Byte]-}
    encode1 p1 p0 =
      let b00 = Primitive.Byte.shiftLeft p1 2 in
      let b01 = Primitive.Byte.shiftRight (Primitive.Byte.and p0 192) 6 in
      let b0 = Primitive.Byte.or 192 (Primitive.Byte.or b00 b01) in
      let b10 = Primitive.Byte.and p0 63 in
      let b1 = Primitive.Byte.or 128 b10 in
      b0 : b1 : []

  {-encode2 ::
        Primitive.Byte.Byte -> Primitive.Byte.Byte ->
          [Primitive.Byte.Byte]-}
    encode2 p1 p0 =
      let b00 = Primitive.Byte.shiftRight (Primitive.Byte.and p1 240) 4 in
      let b0 = Primitive.Byte.or 224 b00 in
      let b10 = Primitive.Byte.shiftLeft (Primitive.Byte.and p1 15) 2 in
      let b11 = Primitive.Byte.shiftRight (Primitive.Byte.and p0 192) 6 in
      let b1 = Primitive.Byte.or 128 (Primitive.Byte.or b10 b11) in
      let b20 = Primitive.Byte.and p0 63 in
      let b2 = Primitive.Byte.or 128 b20 in
      b0 : b1 : b2 : []

  {-encode3 ::
        Primitive.Byte.Byte -> Primitive.Byte.Byte ->
          Primitive.Byte.Byte -> [Primitive.Byte.Byte]-}
    encode3 p p1 p0 =
      let b00 = Primitive.Byte.shiftRight (Primitive.Byte.and p 28) 2 in
      let b0 = Primitive.Byte.or 240 b00 in
      let b10 = Primitive.Byte.shiftLeft (Primitive.Byte.and p 3) 4 in
      let b11 = Primitive.Byte.shiftRight (Primitive.Byte.and p1 240) 4 in
      let b1 = Primitive.Byte.or 128 (Primitive.Byte.or b10 b11) in
      let b20 = Primitive.Byte.shiftLeft (Primitive.Byte.and p1 15) 2 in
      let b21 = Primitive.Byte.shiftRight (Primitive.Byte.and p0 192) 6 in
      let b2 = Primitive.Byte.or 128 (Primitive.Byte.or b20 b21) in
      let b30 = Primitive.Byte.and p0 63 in
      let b3 = Primitive.Byte.or 128 b30 in
      b0 : b1 : b2 : b3 : []

encode :: [Data.Unicode.Unicode] -> [Primitive.Byte.Byte]
encode chs = concat (map encoder chs)

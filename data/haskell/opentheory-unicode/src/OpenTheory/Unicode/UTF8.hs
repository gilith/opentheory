{- |
module: $Header$
description: Unicode characters
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module OpenTheory.Unicode.UTF8
where

import qualified OpenTheory.Natural.Bits as Bits
import qualified OpenTheory.Parser as Parser
import qualified OpenTheory.Parser.Stream as Stream
import qualified OpenTheory.Primitive.Byte as Byte
import qualified OpenTheory.Primitive.Natural as Natural
import qualified OpenTheory.Unicode as Unicode

parseAscii :: Parser.Parser Byte.Byte Natural.Natural
parseAscii =
  Parser.token
    (\b -> if Byte.bit b 7 then Nothing else Just (Byte.toNatural b))

isContinuationByte :: Byte.Byte -> Bool
isContinuationByte b = Byte.bit b 7 && not (Byte.bit b 6)

parseMultibyte :: Parser.Parser Byte.Byte Natural.Natural
parseMultibyte =
  Parser.sequenceParser
    (Parser.token
       (\b ->
          if Byte.bit b 6 then
            if Byte.bit b 5 then
              if Byte.bit b 4 then
                if Byte.bit b 3 then Nothing else Just (parse4 b)
              else Just (parse3 b)
            else Just (parse2 b)
          else Nothing))
  where
  {-parse2 :: Byte.Byte -> Parser.Parser Byte.Byte Natural.Natural-}
    parse2 b =
      Parser.filterParser
        (Parser.foldN addContinuationByte 0
           (Byte.toNatural (Byte.and b 31))) (\n -> 128 <= n)

  {-parse3 :: Byte.Byte -> Parser.Parser Byte.Byte Natural.Natural-}
    parse3 b =
      Parser.filterParser
        (Parser.foldN addContinuationByte 1
           (Byte.toNatural (Byte.and b 15))) (\n -> 2048 <= n)

  {-parse4 :: Byte.Byte -> Parser.Parser Byte.Byte Natural.Natural-}
    parse4 b =
      Parser.filterParser
        (Parser.foldN addContinuationByte 2
           (Byte.toNatural (Byte.and b 7))) (\n -> 65536 <= n)

  {-addContinuationByte ::
        Byte.Byte -> Natural.Natural -> Maybe Natural.Natural-}
    addContinuationByte b n =
      if isContinuationByte b then
        Just (Byte.toNatural (Byte.and b 63) + Natural.shiftLeft n 6)
      else Nothing

parseNatural :: Parser.Parser Byte.Byte Natural.Natural
parseNatural = Parser.orelse parseAscii parseMultibyte

parseUnicode :: Parser.Parser Byte.Byte Unicode.Unicode
parseUnicode =
  Parser.mapPartial parseNatural
    (\n ->
       if Unicode.invariant n then Just (Unicode.Unicode n) else Nothing)

parse :: Parser.Parser Byte.Byte (Either Byte.Byte Unicode.Unicode)
parse =
  Parser.orelse (Parser.mapParser parseUnicode Right)
    (Parser.mapParser Parser.anyToken Left)

decode :: [Byte.Byte] -> [Either Byte.Byte Unicode.Unicode]
decode b = fst (Stream.toList (Parser.parse parse (Stream.fromList b)))

encodeAscii :: Natural.Natural -> [Byte.Byte]
encodeAscii n = Byte.fromNatural n : []

encodeUnicode :: Unicode.Unicode -> [Byte.Byte]
encodeUnicode =
  \c ->
    let n = Unicode.unUnicode c in
    if n < 128 then encodeAscii n
    else if n < 2048 then encode2 n
    else if n < 65536 then encode3 n
    else encode4 n
  where
  {-encode2 :: Natural.Natural -> [Byte.Byte]-}
    encode2 n =
      let n1 = Natural.shiftRight n 6 in
      let b0 = Byte.or 192 (Byte.fromNatural n1) in
      let b1 = Byte.or 128 (Byte.fromNatural (Bits.bound n 6)) in
      b0 : b1 : []

  {-encode3 :: Natural.Natural -> [Byte.Byte]-}
    encode3 n =
      let n1 = Natural.shiftRight n 6 in
      let n2 = Natural.shiftRight n1 6 in
      let b0 = Byte.or 224 (Byte.fromNatural n2) in
      let b1 = Byte.or 128 (Byte.fromNatural (Bits.bound n1 6)) in
      let b2 = Byte.or 128 (Byte.fromNatural (Bits.bound n 6)) in
      b0 : b1 : b2 : []

  {-encode4 :: Natural.Natural -> [Byte.Byte]-}
    encode4 n =
      let n1 = Natural.shiftRight n 6 in
      let n2 = Natural.shiftRight n1 6 in
      let n3 = Natural.shiftRight n2 6 in
      let b0 = Byte.or 240 (Byte.fromNatural n3) in
      let b1 = Byte.or 128 (Byte.fromNatural (Bits.bound n2 6)) in
      let b2 = Byte.or 128 (Byte.fromNatural (Bits.bound n1 6)) in
      let b3 = Byte.or 128 (Byte.fromNatural (Bits.bound n 6)) in
      b0 : b1 : b2 : b3 : []

encode :: [Unicode.Unicode] -> [Byte.Byte]
encode c = concat (map encodeUnicode c)

reencodeUnicode :: Either Byte.Byte Unicode.Unicode -> [Byte.Byte]
reencodeUnicode x =
  case x of
    Left b -> b : []
    Right c -> encodeUnicode c

reencode :: [Either Byte.Byte Unicode.Unicode] -> [Byte.Byte]
reencode c = concat (map reencodeUnicode c)

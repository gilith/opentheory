name: byte-def
version: 1.61
description: Definition of bytes
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2011-07-25
requires: bool
requires: list
requires: natural
requires: natural-bits
requires: natural-divides
requires: pair
requires: probability
show: "Data.Bool"
show: "Data.Byte"
show: "Data.Byte.Bits"
show: "Data.List"
show: "Data.Pair"
show: "Number.Natural"
show: "Probability.Random"

def {
  article: "byte-def.art"
}

word {
  import: def
  interpret: type "Data.Word.word" as "Data.Byte.byte"
  interpret: const "Data.Word.*" as "Data.Byte.*"
  interpret: const "Data.Word.+" as "Data.Byte.+"
  interpret: const "Data.Word.-" as "Data.Byte.-"
  interpret: const "Data.Word.<" as "Data.Byte.<"
  interpret: const "Data.Word.<=" as "Data.Byte.<="
  interpret: const "Data.Word.^" as "Data.Byte.^"
  interpret: const "Data.Word.~" as "Data.Byte.~"
  interpret: const "Data.Word.and" as "Data.Byte.and"
  interpret: const "Data.Word.bit" as "Data.Byte.bit"
  interpret: const "Data.Word.fromNatural" as "Data.Byte.fromNatural"
  interpret: const "Data.Word.fromRandom" as "Data.Byte.fromRandom"
  interpret: const "Data.Word.modulus" as "Data.Byte.modulus"
  interpret: const "Data.Word.not" as "Data.Byte.not"
  interpret: const "Data.Word.or" as "Data.Byte.or"
  interpret: const "Data.Word.shiftLeft" as "Data.Byte.shiftLeft"
  interpret: const "Data.Word.shiftRight" as "Data.Byte.shiftRight"
  interpret: const "Data.Word.toNatural" as "Data.Byte.toNatural"
  interpret: const "Data.Word.width" as "Data.Byte.width"
  interpret: const "Data.Word.Bits.compare" as "Data.Byte.Bits.compare"
  interpret: const "Data.Word.Bits.fromWord" as "Data.Byte.Bits.fromByte"
  interpret: const "Data.Word.Bits.normal" as "Data.Byte.Bits.normal"
  interpret: const "Data.Word.Bits.toWord" as "Data.Byte.Bits.toByte"
  package: word-1.85
}

main {
  import: def
  import: word
}

name: word10-def
version: 1.79
description: Definition of 10-bit words
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
show: "Data.List"
show: "Data.Pair"
show: "Data.Word10"
show: "Data.Word10.Bits"
show: "Number.Natural"
show: "Probability.Random"

def {
  article: "word10-def.art"
}

word {
  import: def
  interpret: type "Data.Word.word" as "Data.Word10.word10"
  interpret: const "Data.Word.*" as "Data.Word10.*"
  interpret: const "Data.Word.+" as "Data.Word10.+"
  interpret: const "Data.Word.-" as "Data.Word10.-"
  interpret: const "Data.Word.<" as "Data.Word10.<"
  interpret: const "Data.Word.<=" as "Data.Word10.<="
  interpret: const "Data.Word.^" as "Data.Word10.^"
  interpret: const "Data.Word.~" as "Data.Word10.~"
  interpret: const "Data.Word.and" as "Data.Word10.and"
  interpret: const "Data.Word.bit" as "Data.Word10.bit"
  interpret: const "Data.Word.fromNatural" as "Data.Word10.fromNatural"
  interpret: const "Data.Word.fromRandom" as "Data.Word10.fromRandom"
  interpret: const "Data.Word.modulus" as "Data.Word10.modulus"
  interpret: const "Data.Word.not" as "Data.Word10.not"
  interpret: const "Data.Word.or" as "Data.Word10.or"
  interpret: const "Data.Word.shiftLeft" as "Data.Word10.shiftLeft"
  interpret: const "Data.Word.shiftRight" as "Data.Word10.shiftRight"
  interpret: const "Data.Word.toNatural" as "Data.Word10.toNatural"
  interpret: const "Data.Word.width" as "Data.Word10.width"
  interpret: const "Data.Word.Bits.compare" as "Data.Word10.Bits.compare"
  interpret: const "Data.Word.Bits.fromWord" as "Data.Word10.Bits.fromWord"
  interpret: const "Data.Word.Bits.normal" as "Data.Word10.Bits.normal"
  interpret: const "Data.Word.Bits.toWord" as "Data.Word10.Bits.toWord"
  package: word-1.103
  checksum: 1ac1057bfd12cf346b4abe9857745c4bf8f566ff
}

main {
  import: def
  import: word
}

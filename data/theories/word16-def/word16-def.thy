name: word16-def
version: 1.97
description: Definition of 16-bit words
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2011-07-25
requires: base
requires: natural-bits
requires: natural-divides
requires: probability
show: "Data.Bool"
show: "Data.List"
show: "Data.Word16"
show: "Data.Word16.Bits"
show: "Number.Natural"
show: "Probability.Random"

def {
  article: "word16-def.art"
}

word {
  import: def
  interpret: type "Data.Word.word" as "Data.Word16.word16"
  interpret: const "Data.Word.*" as "Data.Word16.*"
  interpret: const "Data.Word.+" as "Data.Word16.+"
  interpret: const "Data.Word.-" as "Data.Word16.-"
  interpret: const "Data.Word.<" as "Data.Word16.<"
  interpret: const "Data.Word.<=" as "Data.Word16.<="
  interpret: const "Data.Word.^" as "Data.Word16.^"
  interpret: const "Data.Word.~" as "Data.Word16.~"
  interpret: const "Data.Word.and" as "Data.Word16.and"
  interpret: const "Data.Word.bit" as "Data.Word16.bit"
  interpret: const "Data.Word.fromNatural" as "Data.Word16.fromNatural"
  interpret: const "Data.Word.modulus" as "Data.Word16.modulus"
  interpret: const "Data.Word.not" as "Data.Word16.not"
  interpret: const "Data.Word.or" as "Data.Word16.or"
  interpret: const "Data.Word.random" as "Data.Word16.random"
  interpret: const "Data.Word.shiftLeft" as "Data.Word16.shiftLeft"
  interpret: const "Data.Word.shiftRight" as "Data.Word16.shiftRight"
  interpret: const "Data.Word.toNatural" as "Data.Word16.toNatural"
  interpret: const "Data.Word.width" as "Data.Word16.width"
  interpret: const "Data.Word.Bits.compare" as "Data.Word16.Bits.compare"
  interpret: const "Data.Word.Bits.fromWord" as "Data.Word16.Bits.fromWord"
  interpret: const "Data.Word.Bits.normal" as "Data.Word16.Bits.normal"
  interpret: const "Data.Word.Bits.toWord" as "Data.Word16.Bits.toWord"
  package: word-1.120
}

main {
  import: def
  import: word
}

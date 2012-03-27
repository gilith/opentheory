name: word12-def
version: 1.24
description: Definition of 12-bit words
author: Joe Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2011-07-25
requires: bool
requires: list
requires: natural
requires: natural-divides
show: "Data.Bool"
show: "Data.List"
show: "Data.Word12"
show: "Data.Word12.Bits"
show: "Number.Natural"

def {
  article: "word12-def.art"
}

word {
  import: def
  interpret: type "Data.Word.word" as "Data.Word12.word12"
  interpret: const "Data.Word.*" as "Data.Word12.*"
  interpret: const "Data.Word.+" as "Data.Word12.+"
  interpret: const "Data.Word.-" as "Data.Word12.-"
  interpret: const "Data.Word.<" as "Data.Word12.<"
  interpret: const "Data.Word.<=" as "Data.Word12.<="
  interpret: const "Data.Word.^" as "Data.Word12.^"
  interpret: const "Data.Word.~" as "Data.Word12.~"
  interpret: const "Data.Word.and" as "Data.Word12.and"
  interpret: const "Data.Word.bit" as "Data.Word12.bit"
  interpret: const "Data.Word.fromNatural" as "Data.Word12.fromNatural"
  interpret: const "Data.Word.modulus" as "Data.Word12.modulus"
  interpret: const "Data.Word.not" as "Data.Word12.not"
  interpret: const "Data.Word.or" as "Data.Word12.or"
  interpret: const "Data.Word.shiftLeft" as "Data.Word12.shiftLeft"
  interpret: const "Data.Word.shiftRight" as "Data.Word12.shiftRight"
  interpret: const "Data.Word.toNatural" as "Data.Word12.toNatural"
  interpret: const "Data.Word.width" as "Data.Word12.width"
  interpret: const "Data.Word.Bits.compare" as "Data.Word12.Bits.compare"
  interpret: const "Data.Word.Bits.fromWord" as "Data.Word12.Bits.fromWord"
  interpret: const "Data.Word.Bits.normal" as "Data.Word12.Bits.normal"
  interpret: const "Data.Word.Bits.toWord" as "Data.Word12.Bits.toWord"
  package: word-1.48
}

main {
  import: def
  import: word
}

name: word10
version: 1.16
description: Basic theory of 10-bit words
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Word10"
show: "Data.List"
show: "Number.Natural" as "Natural"

def {
  package: word10-def-1.2
}

word {
  import: def
  interpret: type "Data.Word.word" as "Data.Word10.word10"
  interpret: const "Data.Word.*" as "Data.Word10.*"
  interpret: const "Data.Word.+" as "Data.Word10.+"
  interpret: const "Data.Word.-" as "Data.Word10.-"
  interpret: const "Data.Word.<" as "Data.Word10.<"
  interpret: const "Data.Word.<=" as "Data.Word10.<="
  interpret: const "Data.Word.~" as "Data.Word10.~"
  interpret: const "Data.Word.and" as "Data.Word10.and"
  interpret: const "Data.Word.bit" as "Data.Word10.bit"
  interpret: const "Data.Word.fromNatural" as "Data.Word10.fromNatural"
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
  package: word-1.17
}

bits {
  import: def
  import: word
  package: word10-bits-1.15
}

main {
  import: def
  import: word
  import: bits
}

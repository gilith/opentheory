name: word16
version: 1.19
description: Theory of 16-bit words
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Word16"
show: "Data.List"
show: "Number.Natural" as "Natural"

def {
  package: word16-def-1.2
}

word {
  import: def
  interpret: type "Data.Word.word" as "Data.Word16.word16"
  interpret: const "Data.Word.*" as "Data.Word16.*"
  interpret: const "Data.Word.+" as "Data.Word16.+"
  interpret: const "Data.Word.-" as "Data.Word16.-"
  interpret: const "Data.Word.<" as "Data.Word16.<"
  interpret: const "Data.Word.<=" as "Data.Word16.<="
  interpret: const "Data.Word.~" as "Data.Word16.~"
  interpret: const "Data.Word.and" as "Data.Word16.and"
  interpret: const "Data.Word.bit" as "Data.Word16.bit"
  interpret: const "Data.Word.fromNatural" as "Data.Word16.fromNatural"
  interpret: const "Data.Word.modulus" as "Data.Word16.modulus"
  interpret: const "Data.Word.not" as "Data.Word16.not"
  interpret: const "Data.Word.or" as "Data.Word16.or"
  interpret: const "Data.Word.shiftLeft" as "Data.Word16.shiftLeft"
  interpret: const "Data.Word.shiftRight" as "Data.Word16.shiftRight"
  interpret: const "Data.Word.toNatural" as "Data.Word16.toNatural"
  interpret: const "Data.Word.width" as "Data.Word16.width"
  interpret: const "Data.Word.Bits.compare" as "Data.Word16.Bits.compare"
  interpret: const "Data.Word.Bits.fromWord" as "Data.Word16.Bits.fromWord"
  interpret: const "Data.Word.Bits.normal" as "Data.Word16.Bits.normal"
  interpret: const "Data.Word.Bits.toWord" as "Data.Word16.Bits.toWord"
  package: word-1.15
}

bits {
  import: def
  import: word
  package: word16-bits-1.17
}

bytes {
  import: def
  import: word
  import: bits
  package: word16-bytes-1.16
}

main {
  import: def
  import: word
  import: bits
  import: bytes
}

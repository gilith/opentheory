name: word12
version: 1.9
description: Basic theory of 12-bit words
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Data.Word12"
show: "Number.Natural" as "Natural"

def {
  package: word12-def-1.1
}

word {
  import: def
  interpret: type "Data.Word.word" as "Data.Word12.word12"
  interpret: const "Data.Word.*" as "Data.Word12.*"
  interpret: const "Data.Word.+" as "Data.Word12.+"
  interpret: const "Data.Word.-" as "Data.Word12.-"
  interpret: const "Data.Word.<" as "Data.Word12.<"
  interpret: const "Data.Word.<=" as "Data.Word12.<="
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
  package: word-1.11
}

bits {
  import: def
  import: word
  package: word12-bits-1.11
}

main {
  import: def
  import: word
  import: bits
}

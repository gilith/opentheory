name: word
version: 1.9
description: Parametric theory of words
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Data.Word"
show: "Number.Numeral"

def {
  package: word-def-1.0
}

modular {
  import: def
  interpret: type "Number.Modular.modular" as "Data.Word.word"
  interpret: const "Number.Modular.*" as "Data.Word.*"
  interpret: const "Number.Modular.+" as "Data.Word.+"
  interpret: const "Number.Modular.-" as "Data.Word.-"
  interpret: const "Number.Modular.<" as "Data.Word.<"
  interpret: const "Number.Modular.<=" as "Data.Word.<="
  interpret: const "Number.Modular.~" as "Data.Word.~"
  interpret: const "Number.Modular.fromNatural" as "Data.Word.fromNatural"
  interpret: const "Number.Modular.modulus" as "Data.Word.modulus"
  interpret: const "Number.Modular.toNatural" as "Data.Word.toNatural"
  package: modular-1.9
}

bits {
  import: def
  import: modular
  package: word-bits-1.9
}

main {
  import: def
  import: modular
  import: bits
}

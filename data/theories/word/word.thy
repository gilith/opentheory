name: word
version: 1.5
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
  interpret: const "Number.Modular.size" as "Data.Word.size"
  interpret: const "Number.Modular.toNatural" as "Data.Word.toNatural"
  package: modular-1.0
}

bits {
  import: modular
  package: word-bits-1.5
}

main {
  import: def
  import: modular
  import: bits
}

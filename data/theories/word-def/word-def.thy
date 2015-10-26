name: word-def
version: 1.78
description: Definition of word operations
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2014-11-17
requires: base
requires: natural-bits
requires: natural-divides
requires: probability
requires: word-witness
show: "Data.Bool"
show: "Data.Word"
show: "Number.Natural"
show: "Probability.Random"

def {
  article: "word-def.art"
}

modular {
  import: def
  interpret: type "Number.Modular.modular" as "Data.Word.word"
  interpret: const "Number.Modular.*" as "Data.Word.*"
  interpret: const "Number.Modular.+" as "Data.Word.+"
  interpret: const "Number.Modular.-" as "Data.Word.-"
  interpret: const "Number.Modular.<" as "Data.Word.<"
  interpret: const "Number.Modular.<=" as "Data.Word.<="
  interpret: const "Number.Modular.^" as "Data.Word.^"
  interpret: const "Number.Modular.~" as "Data.Word.~"
  interpret: const "Number.Modular.fromNatural" as "Data.Word.fromNatural"
  interpret: const "Number.Modular.modulus" as "Data.Word.modulus"
  interpret: const "Number.Modular.random" as "Data.Word.random"
  interpret: const "Number.Modular.toNatural" as "Data.Word.toNatural"
  package: modular-1.91
}

main {
  import: def
  import: modular
}

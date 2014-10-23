name: word-def
version: 1.66
description: Definition of word operations
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2014-10-22
requires: bool
requires: natural
requires: natural-bits
requires: natural-divides
requires: pair
requires: probability
requires: word-witness
show: "Data.Bool"
show: "Data.Pair"
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
  interpret: const "Number.Modular.fromRandom" as "Data.Word.fromRandom"
  interpret: const "Number.Modular.modulus" as "Data.Word.modulus"
  interpret: const "Number.Modular.toNatural" as "Data.Word.toNatural"
  package: modular-1.80
  checksum: 1d27090fafec1f052e4617967cc3cf6c927875aa
}

main {
  import: def
  import: modular
}

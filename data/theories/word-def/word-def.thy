name: word-def
version: 1.53
description: Definition of word operations
author: Joe Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-03-26
requires: bool
requires: natural
requires: natural-bits
requires: natural-divides
requires: pair
requires: probability
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
  package: modular-1.68
}

main {
  import: def
  import: modular
}

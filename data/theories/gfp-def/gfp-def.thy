name: gfp-def
version: 1.65
description: Definition of GF(p) finite fields
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2014-11-01
requires: bool
requires: gfp-witness
requires: natural
requires: natural-bits
requires: natural-divides
requires: natural-prime
show: "Data.Bool"
show: "Data.Pair"
show: "Number.GF(p)"
show: "Number.Natural"
show: "Probability.Random"

def {
  article: "gfp-def.art"
}

modular {
  import: def
  interpret: type "Number.Modular.modular" as "Number.GF(p).gfp"
  interpret: const "Number.Modular.*" as "Number.GF(p).*"
  interpret: const "Number.Modular.+" as "Number.GF(p).+"
  interpret: const "Number.Modular.-" as "Number.GF(p).-"
  interpret: const "Number.Modular.<" as "Number.GF(p).<"
  interpret: const "Number.Modular.<=" as "Number.GF(p).<="
  interpret: const "Number.Modular.^" as "Number.GF(p).^"
  interpret: const "Number.Modular.~" as "Number.GF(p).~"
  interpret: const "Number.Modular.fromNatural" as "Number.GF(p).fromNatural"
  interpret: const "Number.Modular.fromRandom" as "Number.GF(p).fromRandom"
  interpret: const "Number.Modular.modulus" as "Number.GF(p).oddprime"
  interpret: const "Number.Modular.toNatural" as "Number.GF(p).toNatural"
  package: modular-1.83
  checksum: 97bd4cdd379ab999b10c773ff3a258afc1a76973
}

main {
  import: def
  import: modular
}

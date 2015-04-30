name: gfp-def
version: 1.71
description: Definition of GF(p) finite fields
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2014-11-01
requires: base
requires: gfp-witness
requires: natural-bits
requires: natural-divides
requires: natural-prime
show: "Data.Bool"
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
  interpret: const "Number.Modular.random" as "Number.GF(p).random"
  interpret: const "Number.Modular.modulus" as "Number.GF(p).oddprime"
  interpret: const "Number.Modular.toNatural" as "Number.GF(p).toNatural"
  package: modular-1.88
}

main {
  import: def
  import: modular
}

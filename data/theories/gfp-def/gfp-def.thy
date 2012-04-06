name: gfp-def
version: 1.30
description: Definition of GF(p) finite fields
author: Joe Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2011-11-15
requires: bool
requires: gfp-witness
requires: natural
requires: natural-divides
requires: natural-prime
show: "Data.Bool"
show: "Number.GF(p)"
show: "Number.Natural"

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
  interpret: const "Number.Modular.modulus" as "Number.GF(p).oddprime"
  interpret: const "Number.Modular.toNatural" as "Number.GF(p).toNatural"
  package: modular-1.50
}

main {
  import: def
  import: modular
}

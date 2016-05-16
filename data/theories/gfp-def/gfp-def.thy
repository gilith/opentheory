name: gfp-def
version: 1.76
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
  interpretation: "modular.int"
  package: modular-1.92
}

main {
  import: def
  import: modular
}

name: gfp
version: 1.14
description: Parametric theory of GF(p) finite fields
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: pair
requires: natural
requires: relation
requires: natural-divides
requires: natural-gcd
requires: natural-prime
requires: gfp-witness
show: "Data.Bool"
show: "Data.Pair"
show: "Number.GF(p)"
show: "Number.Natural"

def {
  package: gfp-def-1.5
}

thm {
  import: def
  package: gfp-thm-1.8
}

div {
  import: def
  import: thm
  package: gfp-div-1.13
}

main {
  import: def
  import: thm
  import: div
}

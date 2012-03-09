name: gfp
version: 1.31
description: Parametric theory of GF(p) finite fields
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: gfp-witness
requires: list
requires: natural
requires: natural-divides
requires: natural-fibonacci
requires: natural-gcd
requires: natural-prime
requires: pair
requires: relation
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Number.GF(p)"
show: "Number.Natural"
show: "Number.Natural.Fibonacci"

def {
  package: gfp-def-1.17
}

thm {
  import: def
  package: gfp-thm-1.18
}

div {
  import: def
  import: thm
  package: gfp-div-1.28
}

main {
  import: def
  import: thm
  import: div
}

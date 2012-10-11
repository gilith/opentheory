name: gfp
version: 1.71
description: Parametric theory of GF(p) finite fields
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: gfp-witness
requires: list
requires: natural
requires: natural-bits
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
show: "Probability.Random"

def {
  package: gfp-def-1.53
}

thm {
  import: def
  package: gfp-thm-1.50
}

div {
  import: def
  import: thm
  package: gfp-div-1.62
}

main {
  import: def
  import: thm
  import: div
}

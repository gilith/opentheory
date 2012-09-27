name: gfp-div
version: 1.57
description: GF(p) field division
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: gfp-def
requires: gfp-thm
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
  package: gfp-div-def-1.52
}

thm {
  import: def
  package: gfp-div-thm-1.51
}

gcd {
  import: def
  import: thm
  package: gfp-div-gcd-1.46
}

exp {
  import: def
  import: thm
  package: gfp-div-exp-1.26
}

main {
  import: def
  import: thm
  import: gcd
  import: exp
}

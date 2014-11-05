name: gfp
version: 1.85
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
  package: gfp-def-1.65
  checksum: d68c1d998a4e9b86342bab5f86e90bc42f82f84f
}

thm {
  import: def
  package: gfp-thm-1.57
  checksum: 60879b7b86b1d7309468187f6b9927b4e9c7af9a
}

div {
  import: def
  import: thm
  package: gfp-div-1.74
  checksum: 6d6b5df2e1bc5f2724c03f6f45dc8e34493b57c6
}

main {
  import: def
  import: thm
  import: div
}

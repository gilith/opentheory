name: gfp
version: 1.84
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
  package: gfp-def-1.64
  checksum: 508963defd60175384b268d149862e0ecc0e9ac6
}

thm {
  import: def
  package: gfp-thm-1.57
  checksum: 60879b7b86b1d7309468187f6b9927b4e9c7af9a
}

div {
  import: def
  import: thm
  package: gfp-div-1.73
  checksum: 8ad08d071dc8466a6bf77ed23ae17116d50da375
}

main {
  import: def
  import: thm
  import: div
}

name: gfp
version: 1.86
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
  package: gfp-def-1.66
  checksum: 0f097e66a2f93616aefe72f38a77842caceec525
}

thm {
  import: def
  package: gfp-thm-1.57
  checksum: 60879b7b86b1d7309468187f6b9927b4e9c7af9a
}

div {
  import: def
  import: thm
  package: gfp-div-1.75
  checksum: 908bb8948e0791ae45d4fb95d7b447ab8b2c583b
}

main {
  import: def
  import: thm
  import: div
}

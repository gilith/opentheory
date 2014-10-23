name: gfp
version: 1.82
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
  package: gfp-def-1.62
  checksum: d914a87ddf7ef7ff542e6a4ea22e508d4c5f2879
}

thm {
  import: def
  package: gfp-thm-1.55
  checksum: dc40c5414bf0b53bfd4944ba347d36bddbcd69f4
}

div {
  import: def
  import: thm
  package: gfp-div-1.71
  checksum: 786c419e74da7c899e829a1cb4c34592db04b7de
}

main {
  import: def
  import: thm
  import: div
}

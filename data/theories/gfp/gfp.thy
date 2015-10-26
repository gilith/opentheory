name: gfp
version: 1.97
description: Parametric theory of GF(p) finite fields
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: gfp-witness
requires: natural-bits
requires: natural-divides
requires: natural-fibonacci
requires: natural-prime
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Number.GF(p)"
show: "Number.Natural"
show: "Number.Natural.Fibonacci"
show: "Probability.Random"
hol-light-int-file: hol-light.int
hol-light-thm-file: hol-light.art

def {
  package: gfp-def-1.75
}

thm {
  import: def
  package: gfp-thm-1.58
}

div {
  import: def
  import: thm
  package: gfp-div-1.78
}

main {
  import: def
  import: thm
  import: div
}

name: gfp-div-gcd
version: 1.65
description: A GF(p) division algorithm based on gcd
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: gfp-def
requires: gfp-div-def
requires: gfp-div-thm
requires: gfp-thm
requires: gfp-witness
requires: natural-gcd
requires: natural-prime
show: "Data.Bool"
show: "Data.Pair"
show: "Number.GF(p)"
show: "Number.Natural"

def {
  package: gfp-div-gcd-def-1.60
}

thm {
  import: def
  package: gfp-div-gcd-thm-1.64
}

main {
  import: def
  import: thm
}

name: gfp-div-gcd
version: 1.29
description: A GF(p) division algorithm based on gcd
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: gfp-def
requires: gfp-div-def
requires: gfp-div-thm
requires: gfp-thm
requires: gfp-witness
requires: natural
requires: natural-gcd
requires: natural-prime
requires: pair
requires: relation
show: "Data.Bool"
show: "Data.Pair"
show: "Number.GF(p)"
show: "Number.Natural"

def {
  package: gfp-div-gcd-def-1.27
}

thm {
  import: def
  package: gfp-div-gcd-thm-1.31
}

main {
  import: def
  import: thm
}

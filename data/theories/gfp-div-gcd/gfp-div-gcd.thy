name: gfp-div-gcd
version: 1.20
description: A GF(p) division algorithm based on gcd
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: pair
requires: natural
requires: relation
requires: natural-gcd
requires: natural-prime
requires: gfp-witness
requires: gfp-def
requires: gfp-thm
requires: gfp-div-def
requires: gfp-div-thm
show: "Data.Bool"
show: "Data.Pair"
show: "Number.GF(p)"
show: "Number.Natural"

def {
  package: gfp-div-gcd-def-1.19
}

thm {
  import: def
  package: gfp-div-gcd-thm-1.22
}

main {
  import: def
  import: thm
}

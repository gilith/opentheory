name: gfp-div-gcd
version: 1.60
description: A GF(p) division algorithm based on gcd
author: Joe Leslie-Hurd <joe@gilith.com>
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
  package: gfp-div-gcd-def-1.56
  checksum: f3a41bcb8c6a83f94eaa721c102f66a8fa853413
}

thm {
  import: def
  package: gfp-div-gcd-thm-1.60
  checksum: 1df5034d1807635d4d11bb8bc707432d4ab25ed3
}

main {
  import: def
  import: thm
}

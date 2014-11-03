name: gfp-div-exp
version: 1.41
description: A GF(p) exponentiation algorithm based on division
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: gfp-def
requires: gfp-div-def
requires: gfp-div-thm
requires: gfp-thm
requires: natural-fibonacci
show: "Data.Bool"
show: "Data.List"
show: "Number.GF(p)"
show: "Number.Natural"
show: "Number.Natural.Fibonacci"

def {
  package: gfp-div-exp-def-1.44
  checksum: 459ac3d7924e30a48f863d08702618499424354b
}

thm {
  import: def
  package: gfp-div-exp-thm-1.42
  checksum: 9967eafcf4549f1925f60727741f5b186d777d26
}

main {
  import: def
  import: thm
}

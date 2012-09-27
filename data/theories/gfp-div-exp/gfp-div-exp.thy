name: gfp-div-exp
version: 1.26
description: A GF(p) exponentiation algorithm based on division
author: Joe Hurd <joe@gilith.com>
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
  package: gfp-div-exp-def-1.31
}

thm {
  import: def
  package: gfp-div-exp-thm-1.29
}

main {
  import: def
  import: thm
}

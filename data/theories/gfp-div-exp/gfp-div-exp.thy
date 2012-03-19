name: gfp-div-exp
version: 1.2
description: A GF(p) exponentiation algorithm based on division
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: base
requires: natural-fibonacci
requires: gfp-def
requires: gfp-thm
requires: gfp-div-def
requires: gfp-div-thm
show: "Data.Bool"
show: "Data.List"
show: "Number.GF(p)"
show: "Number.Natural"
show: "Number.Natural.Fibonacci"

def {
  package: gfp-div-exp-def-1.7
}

thm {
  import: def
  package: gfp-div-exp-thm-1.4
}

main {
  import: def
  import: thm
}

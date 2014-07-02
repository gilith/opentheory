name: gfp-div-exp
version: 1.37
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
  package: gfp-div-exp-def-1.40
}

thm {
  import: def
  package: gfp-div-exp-thm-1.39
}

main {
  import: def
  import: thm
}

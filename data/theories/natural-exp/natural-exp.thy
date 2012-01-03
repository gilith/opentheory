name: natural-exp
version: 1.21
description: Natural number exponentiation
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-def
requires: natural-thm
requires: natural-numeral
requires: natural-order
requires: natural-add
requires: natural-mult
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-exp-def-1.16
}

thm {
  import: def
  package: natural-exp-thm-1.20
}

main {
  import: def
  import: thm
}

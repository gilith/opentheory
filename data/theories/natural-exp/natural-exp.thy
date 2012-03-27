name: natural-exp
version: 1.26
description: Natural number exponentiation
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add
requires: natural-def
requires: natural-mult
requires: natural-numeral
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-exp-def-1.20
}

thm {
  import: def
  package: natural-exp-thm-1.26
}

main {
  import: def
  import: thm
}

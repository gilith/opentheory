name: natural-even-odd
version: 1.11
description: Even and odd natural numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-def
requires: natural-thm
requires: natural-numeral
requires: natural-order
requires: natural-add
requires: natural-mult
requires: natural-exp
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-even-odd-def-1.8
}

thm {
  import: def
  package: natural-even-odd-thm-1.10
}

main {
  import: def
  import: thm
}

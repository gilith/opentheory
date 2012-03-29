name: natural-factorial
version: 1.21
description: Natural number factorial
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
  package: natural-factorial-def-1.17
}

thm {
  import: def
  package: natural-factorial-thm-1.19
}

main {
  import: def
  import: thm
}

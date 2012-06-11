name: natural-factorial
version: 1.22
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
  package: natural-factorial-def-1.18
}

thm {
  import: def
  package: natural-factorial-thm-1.20
}

main {
  import: def
  import: thm
}

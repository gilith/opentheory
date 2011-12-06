name: natural-mult
version: 1.30
description: Natural number multiplication
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-def
requires: natural-thm
requires: natural-numeral
requires: natural-order
requires: natural-add
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-mult-def-1.11
}

thm {
  import: def
  package: natural-mult-thm-1.24
}

main {
  import: def
  import: thm
}

name: natural-mult
version: 1.51
description: Natural number multiplication
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add
requires: natural-def
requires: natural-numeral
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-mult-def-1.22
}

thm {
  import: def
  package: natural-mult-thm-1.46
}

main {
  import: def
  import: thm
}

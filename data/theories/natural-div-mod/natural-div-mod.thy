name: natural-div-mod
version: 1.12
description: Natural number division
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-def
requires: natural-numeral
requires: natural-order
requires: natural-add
requires: natural-mult
requires: natural-exp
requires: natural-even-odd
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-div-mod-def-1.11
}

thm {
  import: def
  package: natural-div-mod-thm-1.12
}

main {
  import: def
  import: thm
}

name: natural-div
version: 1.28
description: Natural number division
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add
requires: natural-def
requires: natural-exp
requires: natural-mult
requires: natural-numeral
requires: natural-order
requires: natural-sub
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-div-def-1.29
}

thm {
  import: def
  package: natural-div-thm-1.32
}

main {
  import: def
  import: thm
}

name: natural-div
version: 1.17
description: Natural number division
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
requires: natural-sub
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-div-def-1.20
}

thm {
  import: def
  package: natural-div-thm-1.20
}

main {
  import: def
  import: thm
}

name: natural-div
version: 1.45
description: Natural number division
author: Joe Leslie-Hurd <joe@gilith.com>
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
  package: natural-div-def-1.36
}

thm {
  import: def
  package: natural-div-thm-1.49
}

main {
  import: def
  import: thm
}

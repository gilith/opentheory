name: natural-distance
version: 1.23
description: Natural number distance
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-def
requires: natural-thm
requires: natural-numeral
requires: natural-order
requires: natural-add
requires: natural-mult
requires: natural-sub
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-distance-def-1.20
}

thm {
  import: def
  package: natural-distance-thm-1.29
}

main {
  import: def
  import: thm
}

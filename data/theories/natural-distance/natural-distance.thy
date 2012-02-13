name: natural-distance
version: 1.30
description: Natural number distance
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-thm
requires: natural-numeral
requires: natural-order
requires: natural-add
requires: natural-mult
requires: natural-sub
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-distance-def-1.23
}

thm {
  import: def
  package: natural-distance-thm-1.37
}

main {
  import: def
  import: thm
}

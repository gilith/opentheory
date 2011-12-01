name: natural-distance
version: 1.21
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
  package: natural-distance-def-1.18
}

thm {
  import: def
  package: natural-distance-thm-1.27
}

main {
  import: def
  import: thm
}

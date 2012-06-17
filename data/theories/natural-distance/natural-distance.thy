name: natural-distance
version: 1.38
description: Natural number distance
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add
requires: natural-mult
requires: natural-numeral
requires: natural-order
requires: natural-sub
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-distance-def-1.30
}

thm {
  import: def
  package: natural-distance-thm-1.45
}

main {
  import: def
  import: thm
}

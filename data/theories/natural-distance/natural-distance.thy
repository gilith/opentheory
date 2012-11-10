name: natural-distance
version: 1.46
description: Natural number distance
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add
requires: natural-mult
requires: natural-numeral
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-distance-def-1.38
}

thm {
  import: def
  package: natural-distance-thm-1.53
}

main {
  import: def
  import: thm
}

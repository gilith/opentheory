name: natural-bits
version: 1.45
description: Natural number to bit-list conversions
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list
requires: natural
requires: pair
requires: probability
requires: relation
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Probability.Random"

def {
  package: natural-bits-def-1.24
  checksum: 9d2141cdf3d83b0933709b76a4c3e41124fd7173
}

thm {
  import: def
  package: natural-bits-thm-1.41
  checksum: 7447544bbf0d514f14e8217af7ddcc6888cdfb1d
}

main {
  import: def
  import: thm
}

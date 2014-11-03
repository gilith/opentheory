name: natural-bits
version: 1.43
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
  package: natural-bits-def-1.23
  checksum: ffe815affeb0ecf20d8894ddad7733f7e4c1e4a8
}

thm {
  import: def
  package: natural-bits-thm-1.40
  checksum: 10baeb43c13e701962d4d64b6700463611a101a9
}

main {
  import: def
  import: thm
}

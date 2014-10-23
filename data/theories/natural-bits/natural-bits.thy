name: natural-bits
version: 1.41
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
  package: natural-bits-def-1.21
  checksum: e5b8d98afbf13cf1c3f12df3bc4b2b742c67f0bc
}

thm {
  import: def
  package: natural-bits-thm-1.38
  checksum: 49623cbacb672d93633ef60365ffd17e04b5c682
}

main {
  import: def
  import: thm
}

name: word10
version: 1.104
description: 10-bit words
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: natural
requires: natural-bits
requires: natural-divides
requires: pair
requires: probability
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Word10"
show: "Data.Word10.Bits"
show: "Number.Natural"
show: "Probability.Random"

def {
  package: word10-def-1.80
  checksum: fc2b58e8d6d937882680dc2eba4a81d3a0085cef
}

bits {
  import: def
  package: word10-bits-1.77
  checksum: ad4b59c0d99c732d700ce838dbe7666c746f486d
}

main {
  import: def
  import: bits
}

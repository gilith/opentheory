name: word10
version: 1.100
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
  package: word10-def-1.76
  checksum: 2efb387f694c58d0681e536c99d795f9d807c114
}

bits {
  import: def
  package: word10-bits-1.75
  checksum: f24dc2e72cc9d76dc17195175e4907a9aad5c88b
}

main {
  import: def
  import: bits
}

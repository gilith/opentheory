name: byte
version: 1.100
description: Bytes
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
show: "Data.Byte"
show: "Data.Byte.Bits"
show: "Data.List"
show: "Data.Pair"
show: "Number.Natural"
show: "Probability.Random"

def {
  package: byte-def-1.73
}

bits {
  import: def
  package: byte-bits-1.78
}

main {
  import: def
  import: bits
}

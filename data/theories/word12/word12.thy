name: word12
version: 1.79
description: 12-bit words
author: Joe Hurd <joe@gilith.com>
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
show: "Data.Word12"
show: "Data.Word12.Bits"
show: "Number.Natural"
show: "Probability.Random"

def {
  package: word12-def-1.56
}

bits {
  import: def
  package: word12-bits-1.64
}

main {
  import: def
  import: bits
}

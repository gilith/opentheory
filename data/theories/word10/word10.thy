name: word10
version: 1.80
description: 10-bit words
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
show: "Data.Word10"
show: "Data.Word10.Bits"
show: "Number.Natural"
show: "Probability.Random"

def {
  package: word10-def-1.56
}

bits {
  import: def
  package: word10-bits-1.66
}

main {
  import: def
  import: bits
}

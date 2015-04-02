name: word10
version: 1.112
description: 10-bit words
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: natural-bits
requires: natural-divides
requires: probability
show: "Data.Bool"
show: "Data.List"
show: "Data.Word10"
show: "Data.Word10.Bits"
show: "Number.Natural"
show: "Probability.Random"

def {
  package: word10-def-1.88
}

bits {
  import: def
  package: word10-bits-1.78
}

main {
  import: def
  import: bits
}

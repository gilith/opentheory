name: word12
version: 1.115
description: 12-bit words
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: natural-bits
requires: natural-divides
requires: probability
show: "Data.Bool"
show: "Data.List"
show: "Data.Word12"
show: "Data.Word12.Bits"
show: "Number.Natural"
show: "Probability.Random"
hol-light-int-file: hol-light.int

def {
  package: word12-def-1.92
}

bits {
  import: def
  package: word12-bits-1.77
}

main {
  import: def
  import: bits
}

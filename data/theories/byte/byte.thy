name: byte
version: 1.120
description: Bytes
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: natural-bits
requires: natural-divides
requires: probability
show: "Data.Bool"
show: "Data.Byte"
show: "Data.Byte.Bits"
show: "Data.List"
show: "Number.Natural"
show: "Probability.Random"
haskell-int-file: haskell.int
haskell-src-file: haskell.art

def {
  package: byte-def-1.90
}

bits {
  import: def
  package: byte-bits-1.83
}

main {
  import: def
  import: bits
}

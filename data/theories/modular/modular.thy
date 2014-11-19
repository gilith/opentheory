name: modular
version: 1.84
description: Parametric theory of modular arithmetic
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: modular-witness
requires: natural
requires: natural-bits
requires: natural-divides
requires: pair
requires: probability
show: "Data.Bool"
show: "Data.Pair"
show: "Number.Modular"
show: "Number.Natural"
show: "Probability.Random"

def {
  package: modular-def-1.81
  checksum: ee366b5dfe3ef9ead6d2aee9d87a21bb34c32925
}

thm {
  import: def
  package: modular-thm-1.65
  checksum: 41fa44233ecdc56364c0f2be2e9cbddd6c539af1
}

main {
  import: def
  import: thm
}

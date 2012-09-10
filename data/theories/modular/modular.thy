name: modular
version: 1.68
description: Parametric theory of modular arithmetic
author: Joe Hurd <joe@gilith.com>
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
  package: modular-def-1.65
}

thm {
  import: def
  package: modular-thm-1.53
}

main {
  import: def
  import: thm
}

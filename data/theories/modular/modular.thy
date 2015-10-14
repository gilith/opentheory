name: modular
version: 1.89
description: Parametric theory of modular arithmetic
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: modular-witness
requires: natural-bits
requires: natural-divides
requires: probability
show: "Data.Bool"
show: "Number.Modular"
show: "Number.Natural"
show: "Probability.Random"
hol-light-int-file: hol-light.int

def {
  package: modular-def-1.85
}

thm {
  import: def
  package: modular-thm-1.66
}

main {
  import: def
  import: thm
}

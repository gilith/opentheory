name: natural-bits
version: 1.26
description: Natural number to bit-list conversions
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list
requires: natural
requires: pair
requires: probability
requires: relation
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Probability.Random"

def {
  package: natural-bits-def-1.18
}

thm {
  import: def
  package: natural-bits-thm-1.25
}

main {
  import: def
  import: thm
}

name: word
version: 1.90
description: Parametric theory of words
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: natural
requires: natural-bits
requires: natural-divides
requires: pair
requires: probability
requires: word-witness
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Word"
show: "Data.Word.Bits"
show: "Number.Natural"
show: "Probability.Random"

def {
  package: word-def-1.62
}

bits {
  import: def
  package: word-bits-1.81
}

main {
  import: def
  import: bits
}

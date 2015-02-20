name: word
version: 1.105
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
  package: word-def-1.71
}

bits {
  import: def
  package: word-bits-1.93
}

main {
  import: def
  import: bits
}

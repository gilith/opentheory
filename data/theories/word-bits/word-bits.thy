name: word-bits
version: 1.73
description: Word to bit-list conversions
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: natural
requires: natural-bits
requires: pair
requires: probability
requires: word-def
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Word"
show: "Data.Word.Bits"
show: "Number.Natural"
show: "Probability.Random"

def {
  package: word-bits-def-1.66
}

thm {
  import: def
  package: word-bits-thm-1.77
}

main {
  import: def
  import: thm
}

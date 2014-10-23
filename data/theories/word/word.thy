name: word
version: 1.100
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
  package: word-def-1.66
  checksum: f291f11ba59ed32e5df7b6a596ec7c0812e3b154
}

bits {
  import: def
  package: word-bits-1.90
  checksum: 6629c966bdeb030f76695c334606f16ecfb1f904
}

main {
  import: def
  import: bits
}

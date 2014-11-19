name: word
version: 1.104
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
  package: word-def-1.70
  checksum: bb09aaecd8c0a8a5805527d8705728f88b5633b7
}

bits {
  import: def
  package: word-bits-1.93
  checksum: 46a497c0e58ce27dc8c987d306000696b71cc7a3
}

main {
  import: def
  import: bits
}

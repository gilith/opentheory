name: word-bits
version: 1.92
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
  package: word-bits-def-1.80
  checksum: 92f46efe6bd80b0b5ad1ab742fb8a1a5a8f41e77
}

thm {
  import: def
  package: word-bits-thm-1.93
  checksum: 79808821b280bc3930a122ddeb4f4e45509b2878
}

main {
  import: def
  import: thm
}

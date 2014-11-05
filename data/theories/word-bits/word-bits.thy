name: word-bits
version: 1.93
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
  package: word-bits-def-1.81
  checksum: 6b0a20ed0aae7bf2e944c0db9c40d5905bbd90c4
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

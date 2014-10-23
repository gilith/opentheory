name: word-bits
version: 1.90
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
  package: word-bits-def-1.78
  checksum: 350b13dfabc47fef5d80df9b442b25a7ab6fe9bd
}

thm {
  import: def
  package: word-bits-thm-1.91
  checksum: 72f47d1dd04f563e76d9c2d620db9ac3d7617c14
}

main {
  import: def
  import: thm
}

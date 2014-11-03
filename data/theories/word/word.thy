name: word
version: 1.102
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
  package: word-def-1.68
  checksum: 1230e89d334ed0f03986b42d5415926a1f6b0547
}

bits {
  import: def
  package: word-bits-1.92
  checksum: 16c5359021a93131f007c5e5e4b338a9af85370f
}

main {
  import: def
  import: bits
}

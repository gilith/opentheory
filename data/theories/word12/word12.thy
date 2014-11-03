name: word12
version: 1.101
description: 12-bit words
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: natural
requires: natural-bits
requires: natural-divides
requires: pair
requires: probability
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Word12"
show: "Data.Word12.Bits"
show: "Number.Natural"
show: "Probability.Random"

def {
  package: word12-def-1.78
  checksum: c7580d618cc18f2337befd5ac7b8787549253c34
}

bits {
  import: def
  package: word12-bits-1.76
  checksum: 61dad1702f10c8414cc3c39f8ca540beb94bb48f
}

main {
  import: def
  import: bits
}

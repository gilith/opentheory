name: byte
version: 1.106
description: Bytes
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
show: "Data.Byte"
show: "Data.Byte.Bits"
show: "Data.List"
show: "Data.Pair"
show: "Number.Natural"
show: "Probability.Random"

def {
  package: byte-def-1.79
  checksum: 5fb3169acb9d0818667082f205eec182226f87c7
}

bits {
  import: def
  package: byte-bits-1.82
  checksum: 24ae0712164eca3dfc8cfeff7c5781c986372730
}

main {
  import: def
  import: bits
}

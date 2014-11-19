name: byte
version: 1.107
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
  package: byte-def-1.80
  checksum: 0e7624282e593c56c06b996eb007f341d5975ccf
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

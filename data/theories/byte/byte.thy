name: byte
version: 1.103
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
  package: byte-def-1.76
  checksum: 2de3c9d638c231f7cf8c660410fc09652fa14442
}

bits {
  import: def
  package: byte-bits-1.80
  checksum: 9ee5423d50f9c5baf4242d5679a2af0ab667fb3e
}

main {
  import: def
  import: bits
}

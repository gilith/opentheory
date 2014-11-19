name: word16
version: 1.113
description: 16-bit words
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: byte
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
show: "Data.Word16"
show: "Data.Word16.Bits"
show: "Number.Natural"
show: "Probability.Random"

def {
  package: word16-def-1.80
  checksum: cda99deb09259ed27544a09f75face4e3a394677
}

bits {
  import: def
  package: word16-bits-1.79
  checksum: f11712eb895e0fe29b04cf2da15a2306a99b557a
}

bytes {
  import: def
  import: bits
  package: word16-bytes-1.84
  checksum: b156d79356153797a7ddaed5c46f1e1b8b32c7c7
}

main {
  import: def
  import: bits
  import: bytes
}

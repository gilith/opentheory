name: word16
version: 1.55
description: 16-bit words
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: byte
requires: list
requires: natural
requires: natural-divides
requires: pair
show: "Data.Bool"
show: "Data.Byte"
show: "Data.Byte.Bits"
show: "Data.List"
show: "Data.Pair"
show: "Data.Word16"
show: "Data.Word16.Bits"
show: "Number.Natural"

def {
  package: word16-def-1.24
}

bits {
  import: def
  package: word16-bits-1.45
}

bytes {
  import: def
  import: bits
  package: word16-bytes-1.48
}

main {
  import: def
  import: bits
  import: bytes
}

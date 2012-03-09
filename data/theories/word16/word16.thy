name: word16
version: 1.50
description: 16-bit words
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: pair
requires: natural
requires: list
requires: natural-divides
requires: byte
show: "Data.Bool"
show: "Data.Byte"
show: "Data.Byte.Bits"
show: "Data.List"
show: "Data.Pair"
show: "Data.Word16"
show: "Data.Word16.Bits"
show: "Number.Natural"

def {
  package: word16-def-1.19
}

bits {
  import: def
  package: word16-bits-1.40
}

bytes {
  import: def
  import: bits
  package: word16-bytes-1.43
}

main {
  import: def
  import: bits
  import: bytes
}

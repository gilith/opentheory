name: word16-bytes
version: 1.84
description: 16-bit word to byte pair conversions
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: byte
requires: list
requires: natural
requires: pair
requires: word16-bits
requires: word16-def
show: "Data.Bool"
show: "Data.Byte"
show: "Data.Byte.Bits"
show: "Data.List"
show: "Data.Pair"
show: "Data.Word16"
show: "Data.Word16.Bits"
show: "Number.Natural"

def {
  package: word16-bytes-def-1.74
  checksum: df32e886d4c543527951ff4eb4052c47c160ad4e
}

thm {
  import: def
  package: word16-bytes-thm-1.91
  checksum: 74ea2aa911a7e87133d0bdc8d16f6cc8ffd6ea10
}

main {
  import: def
  import: thm
}

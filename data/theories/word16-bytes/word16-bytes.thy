name: word16-bytes
version: 1.34
description: 16-bit word to byte pair conversions
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: pair
requires: natural
requires: list
requires: byte
requires: word16-def
requires: word16-bits
show: "Data.Bool"
show: "Data.Byte"
show: "Data.Byte.Bits"
show: "Data.List"
show: "Data.Pair"
show: "Data.Word16"
show: "Data.Word16.Bits"
show: "Number.Natural"

def {
  package: word16-bytes-def-1.30
}

thm {
  import: def
  package: word16-bytes-thm-1.39
}

main {
  import: def
  import: thm
}

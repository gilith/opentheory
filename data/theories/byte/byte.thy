name: byte
version: 1.37
description: Bytes
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: list
requires: natural-divides
show: "Data.Bool"
show: "Data.Byte"
show: "Data.Byte.Bits"
show: "Data.List"
show: "Number.Natural"

def {
  package: byte-def-1.10
}

bits {
  import: def
  package: byte-bits-1.35
}

main {
  import: def
  import: bits
}

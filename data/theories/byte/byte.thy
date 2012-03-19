name: byte
version: 1.47
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
  package: byte-def-1.20
}

bits {
  import: def
  package: byte-bits-1.44
}

main {
  import: def
  import: bits
}

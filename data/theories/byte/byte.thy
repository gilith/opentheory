name: byte
version: 1.62
description: Bytes
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: natural
requires: natural-divides
show: "Data.Bool"
show: "Data.Byte"
show: "Data.Byte.Bits"
show: "Data.List"
show: "Number.Natural"

def {
  package: byte-def-1.35
}

bits {
  import: def
  package: byte-bits-1.56
}

main {
  import: def
  import: bits
}

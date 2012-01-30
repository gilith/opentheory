name: byte
version: 1.38
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
  package: byte-def-1.11
}

bits {
  import: def
  package: byte-bits-1.36
}

main {
  import: def
  import: bits
}

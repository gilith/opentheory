name: word10
version: 1.36
description: 10-bit words
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: list
requires: natural-divides
show: "Data.Bool"
show: "Data.List"
show: "Data.Word10"
show: "Data.Word10.Bits"
show: "Number.Natural"

def {
  package: word10-def-1.11
}

bits {
  import: def
  package: word10-bits-1.31
}

main {
  import: def
  import: bits
}

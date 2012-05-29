name: word12
version: 1.56
description: 12-bit words
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: natural
requires: natural-divides
show: "Data.Bool"
show: "Data.List"
show: "Data.Word12"
show: "Data.Word12.Bits"
show: "Number.Natural"

def {
  package: word12-def-1.33
}

bits {
  import: def
  package: word12-bits-1.48
}

main {
  import: def
  import: bits
}

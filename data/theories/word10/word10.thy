name: word10
version: 1.57
description: 10-bit words
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: natural
requires: natural-divides
show: "Data.Bool"
show: "Data.List"
show: "Data.Word10"
show: "Data.Word10.Bits"
show: "Number.Natural"

def {
  package: word10-def-1.33
}

bits {
  import: def
  package: word10-bits-1.49
}

main {
  import: def
  import: bits
}

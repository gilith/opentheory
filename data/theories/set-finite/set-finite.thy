name: set-finite
version: 1.52
description: Finite sets
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: natural
requires: pair
requires: set-def
requires: set-thm
show: "Data.Bool"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: set-finite-def-1.34
  checksum: bc90f0133f4266131e9d4e6d708c723e0e7d9a39
}

thm {
  import: def
  package: set-finite-thm-1.58
  checksum: abb5aa77c2bcfd3c091d2b10735f31c1d413ab10
}

main {
  import: def
  import: thm
}

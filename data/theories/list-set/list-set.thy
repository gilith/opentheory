name: list-set
version: 1.51
description: List to set conversions
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-dest
requires: list-length
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-set-def-1.50
  checksum: 4df855e1d64968249c3f892171344436ccab2a1d
}

thm {
  import: def
  package: list-set-thm-1.48
  checksum: 533742fff329939d987f55d9699b05d2f242346f
}

main {
  import: def
  import: thm
}

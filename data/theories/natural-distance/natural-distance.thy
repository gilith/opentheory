name: natural-distance
version: 1.52
description: Natural number distance
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add
requires: natural-mult
requires: natural-numeral
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-distance-def-1.41
  checksum: df1b5116f557dfdff8a045727476cd3d4acd62a3
}

thm {
  import: def
  package: natural-distance-thm-1.59
  checksum: 58ae1e3461ae7358e92803981f68dbb713388cba
}

main {
  import: def
  import: thm
}

name: natural-order-min-max
version: 1.36
description: Natural number min and max functions
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-order-def
requires: natural-order-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-order-min-max-def-1.27
  checksum: 36eaff820154891f36bd6942d7276608cd67c00d
}

thm {
  import: def
  package: natural-order-min-max-thm-1.31
  checksum: 68f3fc88ccc6efba002e1f22e2730d3db11d5a01
}

main {
  import: def
  import: thm
}

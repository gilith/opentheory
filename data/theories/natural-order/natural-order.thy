name: natural-order
version: 1.47
description: Natural number orderings
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-def
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-order-def-1.29
  checksum: e0da1143628a7397250cbc23d21bb6b2d5b44adb
}

thm {
  import: def
  package: natural-order-thm-1.39
  checksum: 05b467096ac74cc1b8f230a1c5f50a56c6a06ee4
}

min-max {
  import: def
  import: thm
  package: natural-order-min-max-1.36
  checksum: 425e855850a8cf4b1f4f9ab8d3cc4d5c17c6999c
}

main {
  import: def
  import: thm
  import: min-max
}

name: pair
version: 1.27
description: Product types
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Data.Pair"

def {
  package: pair-def-1.24
  checksum: 0b8284a82166c2c2a0228c493c551fc3c497d43c
}

thm {
  import: def
  package: pair-thm-1.28
  checksum: 562680355d163ab56fa99d9488bf201e07bb09fb
}

main {
  import: def
  import: thm
}

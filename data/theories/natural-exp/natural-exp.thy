name: natural-exp
version: 1.48
description: Natural number exponentiation
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add
requires: natural-def
requires: natural-div
requires: natural-mult
requires: natural-numeral
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-exp-def-1.32
  checksum: 0cca0b2dc1862f4d56ef3c7f3d0e29a9ece3a858
}

thm {
  import: def
  package: natural-exp-thm-1.43
  checksum: a9f6d0ba4bc5e2658f062a33d843374420b96744
}

log {
  import: def
  import: thm
  package: natural-exp-log-1.8
  checksum: 81ecd6208c526d36c105518e8ead3404cc028470
}

main {
  import: def
  import: thm
  import: log
}

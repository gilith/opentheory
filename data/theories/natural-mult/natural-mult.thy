name: natural-mult
version: 1.59
description: Natural number multiplication
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add
requires: natural-def
requires: natural-numeral
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-mult-def-1.28
  checksum: 2f128903c29794f0488dda090038fd73fc6f4ca8
}

thm {
  import: def
  package: natural-mult-thm-1.52
  checksum: 87bcdd7ab588a895e1473a2cd32f8ab3a9b84451
}

main {
  import: def
  import: thm
}

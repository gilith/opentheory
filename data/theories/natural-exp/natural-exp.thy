name: natural-exp
version: 1.51
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
  package: natural-exp-def-1.35
  checksum: e5ac58fcf8e91747fd47878b1ad09dd079cdf46b
}

thm {
  import: def
  package: natural-exp-thm-1.45
  checksum: 9ed926e06f8635164e9bace53ca81a61d2ff51ec
}

log {
  import: def
  import: thm
  package: natural-exp-log-1.10
  checksum: d1d466b1f92a127dd5e4df08c9f073e25f4170c0
}

main {
  import: def
  import: thm
  import: log
}

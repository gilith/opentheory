name: natural-mult
version: 1.61
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
  package: natural-mult-def-1.29
  checksum: 672f62b84ab771104e58ebf817db8652fbd9bffe
}

thm {
  import: def
  package: natural-mult-thm-1.53
  checksum: 4d3440b0e8f76cf3b33f9d671b0f9bc56d782b74
}

main {
  import: def
  import: thm
}

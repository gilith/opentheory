name: natural-mult
version: 1.60
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
  package: natural-mult-thm-1.52
  checksum: 87bcdd7ab588a895e1473a2cd32f8ab3a9b84451
}

main {
  import: def
  import: thm
}

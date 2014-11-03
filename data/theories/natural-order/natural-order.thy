name: natural-order
version: 1.49
description: Natural number orderings
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-def
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-order-def-1.31
  checksum: 9122370469ef878e758aead736bdd76d7ddd5e6a
}

thm {
  import: def
  package: natural-order-thm-1.41
  checksum: 5d29867a5506bb2cf58c3530644e21d00f6f5256
}

min-max {
  import: def
  import: thm
  package: natural-order-min-max-1.38
  checksum: 76b24a07fa1f49b520adcea86f437494d3cd114e
}

main {
  import: def
  import: thm
  import: min-max
}

name: natural-order
version: 1.44
description: Natural number orderings
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-def
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-order-def-1.27
}

thm {
  import: def
  package: natural-order-thm-1.37
}

min-max {
  import: def
  import: thm
  package: natural-order-min-max-1.34
}

main {
  import: def
  import: thm
  import: min-max
}

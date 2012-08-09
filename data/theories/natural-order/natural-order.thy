name: natural-order
version: 1.38
description: Natural number orderings
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-def
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-order-def-1.24
}

thm {
  import: def
  package: natural-order-thm-1.34
}

min-max {
  import: def
  import: thm
  package: natural-order-min-max-1.28
}

main {
  import: def
  import: thm
  import: min-max
}

name: natural-order
version: 1.14
description: Natural number orderings
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-def
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-order-def-1.10
}

thm {
  import: def
  package: natural-order-thm-1.14
}

main {
  import: def
  import: thm
}

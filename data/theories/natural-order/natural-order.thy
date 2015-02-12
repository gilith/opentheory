name: natural-order
version: 1.51
description: Natural number orderings
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-def
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-order-def-1.33
}

thm {
  import: def
  package: natural-order-thm-1.41
}

min-max {
  import: def
  import: thm
  package: natural-order-min-max-1.39
}

main {
  import: def
  import: thm
  import: min-max
}

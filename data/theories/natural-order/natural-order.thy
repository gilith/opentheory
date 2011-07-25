name: natural-order
version: 1.7
description: Definitions and theorems about natural number orderings
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-order-def-1.3
}

thm {
  import: def
  package: natural-order-thm-1.6
}

main {
  import: def
  import: thm
}

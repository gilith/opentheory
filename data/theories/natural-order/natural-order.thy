name: natural-order
version: 1.10
description: Definitions and theorems about natural number orderings
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-order-def-1.6
}

thm {
  import: def
  package: natural-order-thm-1.9
}

main {
  import: def
  import: thm
}

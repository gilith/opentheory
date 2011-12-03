name: natural-order-min-max
version: 1.15
description: Natural number min and max functions
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-order-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-order-min-max-def-1.11
}

thm {
  import: def
  package: natural-order-min-max-thm-1.14
}

main {
  import: def
  import: thm
}

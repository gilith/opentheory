name: natural-order-min-max
version: 1.40
description: Natural number min and max functions
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-order-def
requires: natural-order-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-order-min-max-def-1.29
}

thm {
  import: def
  package: natural-order-min-max-thm-1.35
}

main {
  import: def
  import: thm
}

name: natural-order-min-max
version: 1.13
description: Natural number min and max functions
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-order-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-order-min-max-def-1.9
}

thm {
  import: def
  package: natural-order-min-max-thm-1.12
}

main {
  import: def
  import: thm
}

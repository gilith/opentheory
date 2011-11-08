name: natural-mult
version: 1.12
description: Definitions and theorems about natural number multiplication
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-mult-def-1.6
}

thm {
  import: def
  package: natural-mult-thm-1.5
}

order {
  import: thm
  package: natural-mult-order-1.10
}

main {
  import: def
  import: thm
  import: order
}

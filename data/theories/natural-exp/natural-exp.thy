name: natural-exp
version: 1.5
description: Definitions and theorems about natural number exponentiation
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-exp-def-1.4
}

thm {
  import: def
  package: natural-exp-thm-1.3
}

order {
  import: def
  import: thm
  package: natural-exp-order-1.4
}

main {
  import: def
  import: thm
  import: order
}

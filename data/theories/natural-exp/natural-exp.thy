name: natural-exp
version: 1.6
description: Definitions and theorems about natural number exponentiation
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-exp-def-1.5
}

thm {
  import: def
  package: natural-exp-thm-1.4
}

order {
  import: def
  import: thm
  package: natural-exp-order-1.5
}

main {
  import: def
  import: thm
  import: order
}

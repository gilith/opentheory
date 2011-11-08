name: natural-exp
version: 1.7
description: Definitions and theorems about natural number exponentiation
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-exp-def-1.6
}

thm {
  import: def
  package: natural-exp-thm-1.5
}

order {
  import: def
  import: thm
  package: natural-exp-order-1.6
}

main {
  import: def
  import: thm
  import: order
}

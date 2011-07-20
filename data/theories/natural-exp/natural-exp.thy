name: natural-exp
version: 1.4
description: Definitions and theorems about natural number exponentiation
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: natural-exp-def-1.3
}

thm {
  import: def
  package: natural-exp-thm-1.2
}

order {
  import: def
  import: thm
  package: natural-exp-order-1.3
}

main {
  import: def
  import: thm
  import: order
}

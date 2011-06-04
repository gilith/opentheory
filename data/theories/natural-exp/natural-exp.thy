name: natural-exp
version: 1.1
description: Definitions and theorems about natural number exponentiation
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: natural-exp-def-1.1
}

thm {
  import: def
  package: natural-exp-thm-1.1
}

order {
  import: def
  import: thm
  package: natural-exp-order-1.1
}

main {
  import: def
  import: thm
  import: order
}

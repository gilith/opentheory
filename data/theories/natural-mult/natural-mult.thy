name: natural-mult
version: 1.0
description: Definitions and theorems about natural number multiplication
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: natural-mult-def-1.0
}

thm {
  import: def
  package: natural-mult-thm-1.0
}

order {
  import: thm
  package: natural-mult-order-1.0
}

main {
  import: def
  import: thm
  import: order
}

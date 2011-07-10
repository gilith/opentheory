name: natural-mult
version: 1.5
description: Definitions and theorems about natural number multiplication
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: natural-mult-def-1.1
}

thm {
  import: def
  package: natural-mult-thm-1.1
}

order {
  import: thm
  package: natural-mult-order-1.4
}

main {
  import: def
  import: thm
  import: order
}

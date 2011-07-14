name: natural-add
version: 1.8
description: Definitions and theorems about natural number addition
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: natural-add-def-1.1
}

thm {
  import: def
  package: natural-add-thm-1.1
}

numeral {
  import: thm
  package: natural-add-numeral-1.0
}

suc {
  import: thm
  import: numeral
  package: natural-add-suc-1.0
}

order {
  import: thm
  package: natural-add-order-1.8
}

main {
  import: def
  import: thm
  import: numeral
  import: suc
  import: order
}

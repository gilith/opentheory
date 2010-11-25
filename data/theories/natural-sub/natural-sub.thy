name: natural-sub
version: 1.0
description: Definitions and theorems about natural number subtraction
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: natural-sub-def-1.0
}

thm {
  import: def
  package: natural-sub-thm-1.0
}

main {
  import: def
  import: thm
}

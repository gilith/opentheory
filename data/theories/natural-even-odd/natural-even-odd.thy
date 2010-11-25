name: natural-even-odd
version: 1.0
description: Definitions and theorems about natural number even and odd
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: natural-even-odd-def-1.0
}

thm {
  import: def
  package: natural-even-odd-thm-1.0
}

main {
  import: def
  import: thm
}

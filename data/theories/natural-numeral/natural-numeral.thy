name: natural-numeral
version: 1.0
description: Definitions and theorems about natural number numerals
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: natural-numeral-def-1.0
}

thm {
  import: def
  package: natural-numeral-thm-1.0
}

main {
  import: def
  import: thm
}

name: natural-numeral
version: 1.3
description: Definitions and theorems about natural number numerals
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: natural-numeral-def-1.3
}

thm {
  import: def
  package: natural-numeral-thm-1.1
}

main {
  import: def
  import: thm
}

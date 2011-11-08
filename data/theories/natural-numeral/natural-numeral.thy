name: natural-numeral
version: 1.6
description: Definitions and theorems about natural number numerals
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-numeral-def-1.6
}

thm {
  import: def
  package: natural-numeral-thm-1.2
}

main {
  import: def
  import: thm
}

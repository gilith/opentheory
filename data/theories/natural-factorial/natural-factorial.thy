name: natural-factorial
version: 1.1
description: Definitions and theorems about natural number factorial
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: natural-factorial-def-1.1
}

thm {
  import: def
  package: natural-factorial-thm-1.1
}

main {
  import: def
  import: thm
}

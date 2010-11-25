name: natural-fact
version: 1.0
description: Definitions and theorems about natural number factorial
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: natural-fact-def-1.0
}

thm {
  import: def
  package: natural-fact-thm-1.0
}

main {
  import: def
  import: thm
}

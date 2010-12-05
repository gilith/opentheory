name: natural-pre
version: 1.0
description: Definitions and theorems about the natural number pre function
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: natural-pre-def-1.0
}

thm {
  import: def
  package: natural-pre-thm-1.0
}

main {
  import: def
  import: thm
}

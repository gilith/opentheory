name: natural-add
version: 1.0
description: Definitions and theorems about natural number addition
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: natural-add-def-1.0
}

thm {
  import: def
  package: natural-add-thm-1.0
}

suc {
  import: thm
  package: natural-add-suc-1.0
}

main {
  import: def
  import: thm
  import: suc
}

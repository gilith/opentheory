name: modular-equiv
version: 1.0
description: Definitions and theorems about modular equivalence
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: modular-equiv-def-1.0
}

thm {
  import: def
  package: modular-equiv-thm-1.0
}

main {
  import: def
  import: thm
}

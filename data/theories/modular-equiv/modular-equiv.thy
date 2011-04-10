name: modular-equiv
version: 1.2
description: Definitions and theorems about modular equivalence
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"

def {
  package: modular-equiv-def-1.2
}

thm {
  import: def
  package: modular-equiv-thm-1.0
}

main {
  import: def
  import: thm
}

name: modular-equiv
version: 1.11
description: Definitions and theorems about modular equivalence
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"

def {
  package: modular-equiv-def-1.13
}

thm {
  import: def
  package: modular-equiv-thm-1.2
}

main {
  import: def
  import: thm
}

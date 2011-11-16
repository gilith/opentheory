name: modular-equiv
version: 1.16
description: Definitions and theorems about modular equivalence
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"

def {
  package: modular-equiv-def-1.16
}

thm {
  import: def
  package: modular-equiv-thm-1.4
}

main {
  import: def
  import: thm
}

name: modular
version: 1.58
description: Parametric theory of modular arithmetic
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: modular-witness
requires: natural
requires: natural-divides
show: "Data.Bool"
show: "Number.Modular"
show: "Number.Natural"

def {
  package: modular-def-1.55
}

thm {
  import: def
  package: modular-thm-1.45
}

main {
  import: def
  import: thm
}

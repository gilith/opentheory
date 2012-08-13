name: modular
version: 1.66
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
  package: modular-def-1.64
}

thm {
  import: def
  package: modular-thm-1.53
}

main {
  import: def
  import: thm
}

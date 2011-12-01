name: modular
version: 1.26
description: Parametric theory of modular arithmetic
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: natural-divides
requires: modular-witness
show: "Data.Bool"
show: "Number.Modular"
show: "Number.Natural"

def {
  package: modular-def-1.25
}

thm {
  import: def
  package: modular-thm-1.15
}

main {
  import: def
  import: thm
}

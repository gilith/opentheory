name: modular
version: 1.39
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
  package: modular-def-1.38
}

thm {
  import: def
  package: modular-thm-1.26
}

main {
  import: def
  import: thm
}

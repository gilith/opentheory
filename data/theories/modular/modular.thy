name: modular
version: 1.24
description: Parametric theory of modular arithmetic
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: modular-witness
show: "Data.Bool"
show: "Number.Modular"
show: "Number.Natural"

def {
  package: modular-def-1.21
}

thm {
  import: def
  package: modular-thm-1.12
}

main {
  import: def
  import: thm
}

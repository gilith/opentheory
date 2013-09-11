name: monoid-mult
version: 1.4
description: Monoid multiplication
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: monoid-thm
requires: monoid-witness
requires: natural
requires: natural-bits
show: "Algebra.Monoid"
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: monoid-mult-def-1.2
}

thm {
  import: def
  package: monoid-mult-thm-1.2
}

add {
  import: def
  import: thm
  package: monoid-mult-add-1.5
}

main {
  import: def
  import: thm
  import: add
}

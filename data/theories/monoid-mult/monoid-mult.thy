name: monoid-mult
version: 1.13
description: Monoid multiplication
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: monoid-thm
requires: monoid-witness
requires: natural-bits
show: "Algebra.Monoid"
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: monoid-mult-def-1.10
}

thm {
  import: def
  package: monoid-mult-thm-1.7
}

add {
  import: def
  import: thm
  package: monoid-mult-add-1.14
}

main {
  import: def
  import: thm
  import: add
}

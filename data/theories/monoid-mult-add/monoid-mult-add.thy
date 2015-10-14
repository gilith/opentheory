name: monoid-mult-add
version: 1.14
description: Monoid multiplication by repeated addition
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: monoid-mult-def
requires: monoid-mult-thm
requires: monoid-witness
requires: natural-bits
show: "Algebra.Monoid"
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: monoid-mult-add-def-1.9
}

thm {
  import: def
  package: monoid-mult-add-thm-1.11
}

main {
  import: def
  import: thm
}

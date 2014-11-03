name: monoid
version: 1.10
description: Parametric theory of monoids
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: monoid-witness
requires: natural
requires: natural-bits
show: "Algebra.Monoid"
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

thm {
  package: monoid-thm-1.5
  checksum: 924f849f6b2395479b31ede8e6b1061c60dd0d57
}

mult {
  import: thm
  package: monoid-mult-1.10
  checksum: 095939df761e652ca257f92a090d22d3ee66575c
}

main {
  import: thm
  import: mult
}

name: monoid-comm
version: 1.11
description: Commutative monoids
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2013-02-02
requires: bool
requires: list
requires: monoid-comm-witness
requires: natural
requires: natural-bits
show: "Algebra.Monoid"
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

monoid-witness {
  package: monoid-witness-1.8
  checksum: 23d0bd412c5cf41727742881d3a258ec821ed75d
}

monoid-thm {
  import: monoid-witness
  package: monoid-thm-1.5
  checksum: 924f849f6b2395479b31ede8e6b1061c60dd0d57
}

monoid-mult {
  import: monoid-witness
  import: monoid-thm
  package: monoid-mult-1.10
  checksum: 095939df761e652ca257f92a090d22d3ee66575c
}

thm {
  import: monoid-witness
  package: monoid-comm-thm-1.7
  checksum: f37cbe5d981f18bb4ec0949540b58fbe8b6b1fdd
}

mult {
  import: monoid-mult
  package: monoid-comm-mult-1.3
  checksum: 37385dc7938a9bd86b2d6ed895ab907725b8c6e6
}

main {
  import: thm
  import: mult
}

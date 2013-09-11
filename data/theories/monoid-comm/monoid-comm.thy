name: monoid-comm
version: 1.5
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
  package: monoid-witness-1.4
}

monoid-thm {
  import: monoid-witness
  package: monoid-thm-1.2
}

monoid-mult {
  import: monoid-witness
  import: monoid-thm
  package: monoid-mult-1.4
}

thm {
  import: monoid-witness
  package: monoid-comm-thm-1.3
}

mult {
  import: monoid-mult
  package: monoid-comm-mult-1.3
}

main {
  import: thm
  import: mult
}

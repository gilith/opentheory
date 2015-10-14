name: monoid-comm
version: 1.16
description: Commutative monoids
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2015-10-13
requires: base
requires: monoid-comm-witness
requires: natural-bits
show: "Algebra.Monoid"
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
hol-light-int-file: hol-light.int

monoid-witness {
  package: monoid-witness-1.9
}

monoid-thm {
  import: monoid-witness
  package: monoid-thm-1.6
}

monoid-mult {
  import: monoid-witness
  import: monoid-thm
  package: monoid-mult-1.13
}

thm {
  import: monoid-witness
  package: monoid-comm-thm-1.8
}

mult {
  import: monoid-mult
  package: monoid-comm-mult-1.5
}

main {
  import: thm
  import: mult
}

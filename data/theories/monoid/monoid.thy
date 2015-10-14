name: monoid
version: 1.15
description: Parametric theory of monoids
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: monoid-witness
requires: natural-bits
show: "Algebra.Monoid"
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
hol-light-int-file: hol-light.int

thm {
  package: monoid-thm-1.6
}

mult {
  import: thm
  package: monoid-mult-1.13
}

main {
  import: thm
  import: mult
}

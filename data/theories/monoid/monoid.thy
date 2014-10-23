name: monoid
version: 1.8
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
  package: monoid-thm-1.3
  checksum: a721e52a24b71755a3b10d43fdf54e6b70708e89
}

mult {
  import: thm
  package: monoid-mult-1.8
  checksum: 47c9f7650790fbac3af20d89d2959bc12b553374
}

main {
  import: thm
  import: mult
}

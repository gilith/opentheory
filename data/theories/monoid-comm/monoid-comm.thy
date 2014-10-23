name: monoid-comm
version: 1.9
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
  package: monoid-witness-1.6
  checksum: 217b6f5c01439c3f9f11e282efa4007a572d38ef
}

monoid-thm {
  import: monoid-witness
  package: monoid-thm-1.3
  checksum: a721e52a24b71755a3b10d43fdf54e6b70708e89
}

monoid-mult {
  import: monoid-witness
  import: monoid-thm
  package: monoid-mult-1.8
  checksum: 47c9f7650790fbac3af20d89d2959bc12b553374
}

thm {
  import: monoid-witness
  package: monoid-comm-thm-1.5
  checksum: fd30fd08153b66def2a3b70dfee21f9ea2395122
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

name: monoid-mult
version: 1.8
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
  package: monoid-mult-def-1.6
  checksum: adda6ec65af44770790a2eb47c06b08be6c8071c
}

thm {
  import: def
  package: monoid-mult-thm-1.4
  checksum: ee04ea95e180608bdb5a2d0d52b62c7713e0d4f3
}

add {
  import: def
  import: thm
  package: monoid-mult-add-1.9
  checksum: 2a4b18953c0c934532eed89fbf0850cad40e732a
}

main {
  import: def
  import: thm
  import: add
}

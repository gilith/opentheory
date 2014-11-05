name: monoid-mult-add
version: 1.12
description: Monoid multiplication by repeated addition
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: monoid-mult-def
requires: monoid-mult-thm
requires: monoid-witness
requires: natural
requires: natural-bits
show: "Algebra.Monoid"
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: monoid-mult-add-def-1.8
  checksum: 28ae32b6949c8c5e109501f0cfd9b98ce171b31a
}

thm {
  import: def
  package: monoid-mult-add-thm-1.9
  checksum: b82f5e4ff123d5c2b1c156fc6f35a6eac8e5bc7b
}

main {
  import: def
  import: thm
}

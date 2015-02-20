name: monoid-mult-add
version: 1.13
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
}

thm {
  import: def
  package: monoid-mult-add-thm-1.10
}

main {
  import: def
  import: thm
}

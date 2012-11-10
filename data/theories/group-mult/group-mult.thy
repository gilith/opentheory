name: group-mult
version: 1.8
description: Group scalar multiplication
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: group-def
requires: group-thm
requires: group-witness
requires: list
requires: natural
requires: natural-bits
requires: natural-fibonacci
show: "Algebra.Group"
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: group-mult-def-1.8
}

thm {
  import: def
  package: group-mult-thm-1.5
}

add {
  import: def
  import: thm
  package: group-mult-add-1.7
}

sub {
  import: def
  import: thm
  package: group-mult-sub-1.7
}

main {
  import: def
  import: thm
  import: add
  import: sub
}

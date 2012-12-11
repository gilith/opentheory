name: group
version: 1.9
description: Parametric theory of groups
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: group-witness
requires: list
requires: natural
requires: natural-bits
requires: natural-fibonacci
requires: pair
show: "Algebra.Group"
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Number.Natural"

def {
  package: group-def-1.10
}

thm {
  package: group-thm-1.4
}

mult {
  import: def
  import: thm
  package: group-mult-1.9
}

crypt {
  import: thm
  import: mult
  package: group-crypt-1.8
}

abelian {
  import: thm
  package: group-abelian-1.9
}

main {
  import: def
  import: thm
  import: mult
  import: crypt
  import: abelian
}

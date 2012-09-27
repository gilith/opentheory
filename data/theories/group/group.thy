name: group
version: 1.1
description: Parametric theory of groups
author: Joe Hurd <joe@gilith.com>
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
  package: group-def-1.3
}

thm {
  package: group-thm-1.2
}

mult {
  import: def
  import: thm
  package: group-mult-1.2
}

crypt {
  import: thm
  import: mult
  package: group-crypt-1.0
}

abelian {
  import: thm
  package: group-abelian-1.2
}

main {
  import: def
  import: thm
  import: mult
  import: crypt
  import: abelian
}

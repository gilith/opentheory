name: group-crypt
version: 1.6
description: Group cryptography
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: group-mult
requires: group-thm
requires: group-witness
requires: natural
requires: pair
show: "Algebra.Group"
show: "Data.Bool"
show: "Data.Pair"
show: "Number.Natural"

def {
  package: group-crypt-def-1.9
}

thm {
  import: def
  package: group-crypt-thm-1.9
}

main {
  import: def
  import: thm
}

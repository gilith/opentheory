name: group-crypt
version: 1.8
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
  package: group-crypt-def-1.11
}

thm {
  import: def
  package: group-crypt-thm-1.10
}

main {
  import: def
  import: thm
}

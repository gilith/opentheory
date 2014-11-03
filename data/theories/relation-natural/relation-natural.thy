name: relation-natural
version: 1.34
description: Relations over natural numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: natural
requires: relation-def
requires: relation-thm
requires: relation-well-founded
requires: set
show: "Data.Bool"
show: "Function"
show: "Number.Natural"
show: "Relation"

def {
  package: relation-natural-def-1.26
  checksum: bb6564664b7136f9979da87aed52df68a786e00f
}

thm {
  import: def
  package: relation-natural-thm-1.39
  checksum: 03b2e9a3660ffae5943a9c6a3738e446e5fb11e0
}

main {
  import: def
  import: thm
}

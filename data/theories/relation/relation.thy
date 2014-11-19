name: relation
version: 1.59
description: Relation operators
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: natural
requires: pair
requires: set
show: "Data.Bool"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Relation"
show: "Set"

def {
  package: relation-def-1.25
  checksum: 9f7a83d975e6e78994a4c5cbf2a328f27bc1d513
}

thm {
  import: def
  package: relation-thm-1.15
  checksum: d7008776d55f6c2701e81a162a7f24486e8db96a
}

well-founded {
  import: def
  import: thm
  package: relation-well-founded-1.55
  checksum: cb567de3ec06e7e2d1578b9b6ffff8b3fad9f6b3
}

natural {
  import: def
  import: thm
  import: well-founded
  package: relation-natural-1.35
  checksum: b23bcf09d5b239155c6aa887f7387a9f0aaf5d2d
}

main {
  import: def
  import: thm
  import: well-founded
  import: natural
}

name: relation
version: 1.56
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
  package: relation-def-1.24
  checksum: 5aabe99b20c587f5bd80aef82459e3934118b318
}

thm {
  import: def
  package: relation-thm-1.12
  checksum: 5f5499acc883290a7d9adbe9663a3255d4185038
}

well-founded {
  import: def
  import: thm
  package: relation-well-founded-1.52
  checksum: b02f4b63b140964aa83ee69241e73bdfe8edb63b
}

natural {
  import: def
  import: thm
  import: well-founded
  package: relation-natural-1.32
  checksum: 0c9955682835bae864349f750817680d0429125d
}

main {
  import: def
  import: thm
  import: well-founded
  import: natural
}

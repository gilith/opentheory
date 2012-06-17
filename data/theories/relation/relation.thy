name: relation
version: 1.43
description: Relation operators
author: Joe Hurd <joe@gilith.com>
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

def {
  package: relation-def-1.14
}

thm {
  import: def
  package: relation-thm-1.6
}

well-founded {
  import: def
  import: thm
  package: relation-well-founded-1.40
}

natural {
  import: def
  import: thm
  import: well-founded
  package: relation-natural-1.22
}

main {
  import: def
  import: thm
  import: well-founded
  import: natural
}

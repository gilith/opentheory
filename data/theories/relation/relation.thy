name: relation
version: 1.25
description: Relation operators
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: pair
requires: natural
requires: set
show: "Data.Bool"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Relation"

def {
  package: relation-def-1.4
}

thm {
  import: def
  package: relation-thm-1.2
}

well-founded {
  import: def
  import: thm
  package: relation-well-founded-1.26
}

natural {
  import: def
  import: thm
  import: well-founded
  package: relation-natural-1.4
}

main {
  import: def
  import: thm
  import: well-founded
  import: natural
}

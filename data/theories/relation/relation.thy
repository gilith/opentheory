name: relation
version: 1.34
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
  package: relation-def-1.9
}

thm {
  import: def
  package: relation-thm-1.4
}

well-founded {
  import: def
  import: thm
  package: relation-well-founded-1.34
}

natural {
  import: def
  import: thm
  import: well-founded
  package: relation-natural-1.13
}

main {
  import: def
  import: thm
  import: well-founded
  import: natural
}

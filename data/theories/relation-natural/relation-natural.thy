name: relation-natural
version: 1.26
description: Relations over natural numbers
author: Joe Hurd <joe@gilith.com>
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
  package: relation-natural-def-1.21
}

thm {
  import: def
  package: relation-natural-thm-1.31
}

main {
  import: def
  import: thm
}

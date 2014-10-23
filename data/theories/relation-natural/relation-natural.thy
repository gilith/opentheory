name: relation-natural
version: 1.32
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
  package: relation-natural-def-1.25
  checksum: c2f07ed1f278c52f19f5c66ff511347862ebf82a
}

thm {
  import: def
  package: relation-natural-thm-1.37
  checksum: 183d331203dc609063c220a151f30bfb0b750f89
}

main {
  import: def
  import: thm
}

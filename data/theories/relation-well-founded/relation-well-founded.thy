name: relation-well-founded
version: 1.51
description: Well-founded relations
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: pair
requires: relation-def
requires: relation-thm
show: "Data.Bool"
show: "Data.Pair"
show: "Relation"

def {
  package: relation-well-founded-def-1.36
}

thm {
  import: def
  package: relation-well-founded-thm-1.58
}

main {
  import: def
  import: thm
}

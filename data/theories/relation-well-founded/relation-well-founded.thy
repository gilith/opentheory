name: relation-well-founded
version: 1.34
description: Well-founded relations
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: pair
requires: relation-def
requires: relation-thm
show: "Data.Bool"
show: "Data.Pair"
show: "Relation"

def {
  package: relation-well-founded-def-1.21
}

thm {
  import: def
  package: relation-well-founded-thm-1.39
}

main {
  import: def
  import: thm
}

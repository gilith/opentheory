name: relation-well-founded
version: 1.55
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
  package: relation-well-founded-def-1.38
}

thm {
  import: def
  package: relation-well-founded-thm-1.62
}

main {
  import: def
  import: thm
}

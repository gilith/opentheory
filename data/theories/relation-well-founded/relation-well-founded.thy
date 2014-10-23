name: relation-well-founded
version: 1.52
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
  package: relation-well-founded-def-1.37
  checksum: e3f537ef862af496525457cbf2fe8a10985d8615
}

thm {
  import: def
  package: relation-well-founded-thm-1.59
  checksum: 5fde0e2a1af32019dbf3398049de63d8342ab312
}

main {
  import: def
  import: thm
}

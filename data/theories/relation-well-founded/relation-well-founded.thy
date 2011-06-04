name: relation-well-founded
version: 1.2
description: Well-founded relations
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Pair"
show: "Number.Natural"
show: "Relation"

def {
  package: relation-well-founded-def-1.2
}

thm {
  import: def
  package: relation-well-founded-thm-1.2
}

main {
  import: def
  import: thm
}

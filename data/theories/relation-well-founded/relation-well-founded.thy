name: relation-well-founded
version: 1.0
description: Definitions and theorems about well-founded relations
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
show: "Data.Bool"
show: "Relation"

def {
  package: relation-well-founded-def-1.0
}

thm {
  import: def
  package: relation-well-founded-thm-1.0
}

trivial {
  import: def
  package: relation-well-founded-trivial-1.0
}

tail {
  import: thm
  package: relation-well-founded-tail-1.0
}

main {
  import: def
  import: thm
  import: trivial
  import: tail
}

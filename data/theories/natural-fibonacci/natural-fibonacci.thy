name: natural-fibonacci
version: 1.4
description: Fibonacci numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: base
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Number.Natural.Fibonacci"
show: "Relation"

exists {
  package: natural-fibonacci-exists-1.1
}

def {
  import: exists
  package: natural-fibonacci-def-1.2
}

thm {
  import: exists
  import: def
  package: natural-fibonacci-thm-1.4
}

main {
  import: exists
  import: def
  import: thm
}

name: natural-fibonacci
version: 1.11
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
  package: natural-fibonacci-exists-1.4
}

def {
  import: exists
  package: natural-fibonacci-def-1.6
}

thm {
  import: exists
  import: def
  package: natural-fibonacci-thm-1.11
}

main {
  import: def
  import: thm
}

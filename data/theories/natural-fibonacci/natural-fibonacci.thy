name: natural-fibonacci
version: 1.44
description: Fibonacci numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: probability
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Function"
show: "Number.Natural"
show: "Number.Natural.Fibonacci"
show: "Probability.Random"
show: "Relation"

exists {
  package: natural-fibonacci-exists-1.31
}

def {
  import: exists
  package: natural-fibonacci-def-1.35
}

thm {
  import: exists
  import: def
  package: natural-fibonacci-thm-1.40
}

main {
  import: def
  import: thm
}

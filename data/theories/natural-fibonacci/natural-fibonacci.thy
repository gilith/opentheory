name: natural-fibonacci
version: 1.63
description: Fibonacci numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: probability
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Number.Natural"
show: "Number.Natural.Fibonacci"
show: "Probability.Random"
show: "Relation"

exists {
  package: natural-fibonacci-exists-1.39
}

def {
  import: exists
  package: natural-fibonacci-def-1.48
}

thm {
  import: exists
  import: def
  package: natural-fibonacci-thm-1.52
}

main {
  import: def
  import: thm
}

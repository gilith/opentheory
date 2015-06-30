name: natural-fibonacci
version: 1.69
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
haskell-name: opentheory-fibonacci
haskell-category: Number Theory
haskell-int-file: haskell.int
haskell-src-file: haskell.art
haskell-test-file: haskell-test.art

exists {
  package: natural-fibonacci-exists-1.40
}

def {
  import: exists
  package: natural-fibonacci-def-1.50
}

thm {
  import: exists
  import: def
  package: natural-fibonacci-thm-1.57
}

main {
  import: def
  import: thm
}

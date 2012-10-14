name: haskell-prime
version: 1.20
description: Prime numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
haskell-category: Number Theory
requires: base
requires: haskell
requires: natural-divides
requires: natural-prime
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Function"
show: "Haskell.Data.Stream" as "H"
show: "Haskell.Number.Natural" as "H"
show: "Haskell.Number.Natural.Prime.Sieve" as "H"
show: "Number.Natural"
show: "Number.Natural.Prime.Sieve"
show: "Probability.Random"

def {
  package: haskell-prime-def-1.10
}

src {
  import: def
  package: haskell-prime-src-1.16
}

test {
  import: def
  package: haskell-prime-test-1.21
}

main {
  import: def
  import: src
  import: test
}

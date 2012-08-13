name: haskell-prime
version: 1.8
description: Prime numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
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
  package: haskell-prime-def-1.7
}

src {
  import: def
  package: haskell-prime-src-1.8
}

test {
  import: def
  package: haskell-prime-test-1.9
}

main {
  import: def
  import: src
  import: test
}

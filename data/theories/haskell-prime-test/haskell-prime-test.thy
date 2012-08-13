name: haskell-prime-test
version: 1.9
description: QuickCheck tests for prime numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-08-13
requires: base
requires: haskell
requires: haskell-prime-def
requires: natural-prime
requires: natural-divides
show: "Data.Bool"
show: "Data.Pair"
show: "Data.Stream"
show: "Haskell.Data.Stream" as "H"
show: "Haskell.Number.Natural" as "H"
show: "Number.Natural"
show: "Probability.Random"

main {
  article: "haskell-prime-test.art"
}

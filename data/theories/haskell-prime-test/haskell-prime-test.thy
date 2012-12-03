name: haskell-prime-test
version: 1.28
description: QuickCheck tests for prime numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-12-02
requires: base
requires: haskell
requires: haskell-prime-def
requires: natural-divides
requires: natural-prime
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Haskell.Data.Stream" as "H"
show: "Haskell.Number.Natural" as "H"
show: "Number.Natural"
show: "Probability.Random"

main {
  article: "haskell-prime-test.art"
}

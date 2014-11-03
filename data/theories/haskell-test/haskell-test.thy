name: haskell-test
version: 1.45
description: Testing the Haskell base
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2014-11-01
requires: base
requires: haskell-def
requires: probability
show: "Data.Bool"
show: "Data.Pair"
show: "Haskell.Number.Natural" as "H"
show: "Number.Natural"
show: "Probability.Random"

main {
  article: "haskell-test.art"
}

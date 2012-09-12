name: haskell-test
version: 1.26
description: Testing the Haskell base
author: Joe Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-08-13
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

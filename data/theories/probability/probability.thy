name: probability
version: 1.46
description: Probability
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Function"
show: "Number.Natural"
show: "Probability.Random"
haskell-int-file: haskell.int
haskell-src-file: haskell.art
haskell-arbitrary-type: "Probability.Random.random"

def {
  package: probability-def-1.42
}

thm {
  import: def
  package: probability-thm-1.20
}

main {
  import: def
  import: thm
}

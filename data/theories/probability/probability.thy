name: probability
version: 1.42
description: Probability
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Number.Natural"
show: "Probability.Random"
haskell-int-file: haskell.int
haskell-src-file: haskell.art

def {
  package: probability-def-1.41
}

thm {
  import: def
  package: probability-thm-1.17
}

main {
  import: def
  import: thm
}

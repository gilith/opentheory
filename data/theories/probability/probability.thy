name: probability
version: 1.4
description: Probability
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Data.Pair"
show: "Data.Stream"
show: "Probability.Random"

def {
  package: probability-def-1.7
}

main {
  import: def
}

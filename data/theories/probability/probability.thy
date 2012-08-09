name: probability
version: 1.11
description: Probability
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: pair
requires: relation
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Number.Natural"
show: "Probability.Random"

def {
  package: probability-def-1.14
}

main {
  import: def
}
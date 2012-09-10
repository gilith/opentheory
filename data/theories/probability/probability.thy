name: probability
version: 1.24
description: Probability
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: natural
requires: pair
requires: relation
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Number.Natural"
show: "Probability.Random"

def {
  package: probability-def-1.26
}

thm {
  import: def
  package: probability-thm-1.3
}

main {
  import: def
  import: thm
}

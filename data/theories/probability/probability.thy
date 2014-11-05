name: probability
version: 1.37
description: Probability
author: Joe Leslie-Hurd <joe@gilith.com>
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
  package: probability-def-1.38
  checksum: 67ca4acd29d84ae8e2033975d8d7383430d21073
}

thm {
  import: def
  package: probability-thm-1.14
  checksum: ffb4e4b94ab99181a453960396f4a7d929a69f52
}

main {
  import: def
  import: thm
}

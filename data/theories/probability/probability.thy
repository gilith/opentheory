name: probability
version: 1.34
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
  package: probability-def-1.35
  checksum: 69195ad08f08e39b067103feabd5b098eb5890eb
}

thm {
  import: def
  package: probability-thm-1.12
  checksum: 680ac3e63cc05d77ce9af596b42416efa770a48e
}

main {
  import: def
  import: thm
}

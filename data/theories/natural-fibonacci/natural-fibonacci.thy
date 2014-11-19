name: natural-fibonacci
version: 1.56
description: Fibonacci numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: probability
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Function"
show: "Number.Natural"
show: "Number.Natural.Fibonacci"
show: "Probability.Random"
show: "Relation"

exists {
  package: natural-fibonacci-exists-1.38
  checksum: 923afa20aa5d6886d124fa82441c42c7cfd03f14
}

def {
  import: exists
  package: natural-fibonacci-def-1.45
  checksum: 65627936a61fe5b31235d04625b625f5c8f4e7ae
}

thm {
  import: exists
  import: def
  package: natural-fibonacci-thm-1.49
  checksum: 280affe5f1e072e391ffe5b15aed4ff04337cc6d
}

main {
  import: def
  import: thm
}

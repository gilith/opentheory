name: natural-fibonacci
version: 1.55
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
  package: natural-fibonacci-def-1.44
  checksum: 9c213fbf46640fb70ef78bb506c37fd7df8c221b
}

thm {
  import: exists
  import: def
  package: natural-fibonacci-thm-1.48
  checksum: ecfe5810402fe3308def9a4e76f776a72f7db4d7
}

main {
  import: def
  import: thm
}

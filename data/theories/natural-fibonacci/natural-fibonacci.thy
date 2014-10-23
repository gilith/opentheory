name: natural-fibonacci
version: 1.52
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
  package: natural-fibonacci-exists-1.36
  checksum: 4843fb3632d6dc14097e4072678124708668b7c7
}

def {
  import: exists
  package: natural-fibonacci-def-1.41
  checksum: 3e02ad75c0d9d52034854c11f4fd7af1e83ab541
}

thm {
  import: exists
  import: def
  package: natural-fibonacci-thm-1.46
  checksum: 330bf955f4533e06e2e90b9c2de016773c3eda4e
}

main {
  import: def
  import: thm
}

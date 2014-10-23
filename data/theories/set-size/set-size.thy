name: set-size
version: 1.54
description: Finite set cardinality
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: pair
requires: set-def
requires: set-finite
requires: set-fold
requires: set-thm
show: "Data.Bool"
show: "Data.Pair"
show: "Number.Natural"
show: "Set"

def {
  package: set-size-def-1.32
  checksum: c1410ac51e7a0a94777268decd6a7fd65bb0662e
}

thm {
  import: def
  package: set-size-thm-1.60
  checksum: 5250642b52a23c2688c897d85b1ea5d7bc18f390
}

main {
  import: def
  import: thm
}

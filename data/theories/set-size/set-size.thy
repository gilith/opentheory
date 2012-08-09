name: set-size
version: 1.46
description: Finite set cardinality
author: Joe Hurd <joe@gilith.com>
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
  package: set-size-def-1.27
}

thm {
  import: def
  package: set-size-thm-1.53
}

main {
  import: def
  import: thm
}

name: set-size
version: 1.36
description: Finite set cardinality
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: pair
requires: natural
requires: set-def
requires: set-thm
requires: set-finite
requires: set-fold
show: "Data.Bool"
show: "Data.Pair"
show: "Number.Natural"
show: "Set"

def {
  package: set-size-def-1.19
}

thm {
  import: def
  package: set-size-thm-1.41
}

main {
  import: def
  import: thm
}

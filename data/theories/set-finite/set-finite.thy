name: set-finite
version: 1.22
description: Finite sets
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: pair
requires: natural
requires: set-def
requires: set-thm
show: "Data.Bool"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: set-finite-def-1.14
}

thm {
  import: def
  package: set-finite-thm-1.27
}

main {
  import: def
  import: thm
}

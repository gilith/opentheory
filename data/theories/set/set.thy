name: set
version: 1.53
description: Set types
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: natural
requires: pair
show: "Data.Bool"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: set-def-1.43
}

thm {
  import: def
  package: set-thm-1.51
}

finite {
  import: def
  import: thm
  package: set-finite-1.43
}

fold {
  import: thm
  import: finite
  package: set-fold-1.36
}

size {
  import: def
  import: thm
  import: finite
  import: fold
  package: set-size-1.47
}

main {
  import: def
  import: thm
  import: finite
  import: fold
  import: size
}

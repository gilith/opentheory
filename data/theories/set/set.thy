name: set
version: 1.37
description: Set types
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: pair
requires: natural
show: "Data.Bool"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: set-def-1.33
}

thm {
  import: def
  package: set-thm-1.38
}

finite {
  import: def
  import: thm
  package: set-finite-1.32
}

fold {
  import: thm
  import: finite
  package: set-fold-1.28
}

size {
  import: def
  import: thm
  import: finite
  import: fold
  package: set-size-1.35
}

main {
  import: def
  import: thm
  import: finite
  import: fold
  import: size
}

name: set
version: 1.33
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
  package: set-def-1.30
}

thm {
  import: def
  package: set-thm-1.34
}

finite {
  import: def
  import: thm
  package: set-finite-1.29
}

fold {
  import: thm
  import: finite
  package: set-fold-1.25
}

size {
  import: def
  import: thm
  import: finite
  import: fold
  package: set-size-1.31
}

main {
  import: def
  import: thm
  import: finite
  import: fold
  import: size
}

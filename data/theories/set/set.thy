name: set
version: 1.55
description: Set types
author: Joe Leslie-Hurd <joe@gilith.com>
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
  package: set-def-1.44
}

thm {
  import: def
  package: set-thm-1.53
}

finite {
  import: def
  import: thm
  package: set-finite-1.45
}

fold {
  import: thm
  import: finite
  package: set-fold-1.38
}

size {
  import: def
  import: thm
  import: finite
  import: fold
  package: set-size-1.49
}

main {
  import: def
  import: thm
  import: finite
  import: fold
  import: size
}

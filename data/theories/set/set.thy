name: set
version: 1.11
description: Set theory
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: set-def-1.12
}

thm {
  import: def
  package: set-thm-1.12
}

finite {
  import: def
  import: thm
  package: set-finite-1.9
}

fold {
  import: def
  import: thm
  import: finite
  package: set-fold-1.7
}

size {
  import: def
  import: thm
  import: finite
  import: fold
  package: set-size-1.9
}

main {
  import: def
  import: thm
  import: finite
  import: fold
  import: size
}

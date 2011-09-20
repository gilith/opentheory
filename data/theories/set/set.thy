name: set
version: 1.12
description: Set theory
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: set-def-1.13
}

thm {
  import: def
  package: set-thm-1.13
}

finite {
  import: def
  import: thm
  package: set-finite-1.10
}

fold {
  import: def
  import: thm
  import: finite
  package: set-fold-1.8
}

size {
  import: def
  import: thm
  import: finite
  import: fold
  package: set-size-1.10
}

main {
  import: def
  import: thm
  import: finite
  import: fold
  import: size
}

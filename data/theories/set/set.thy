name: set
version: 1.18
description: Standard set theory
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: set-def-1.18
}

thm {
  import: def
  package: set-thm-1.19
}

finite {
  import: def
  import: thm
  package: set-finite-1.15
}

fold {
  import: def
  import: thm
  import: finite
  package: set-fold-1.13
}

size {
  import: def
  import: thm
  import: finite
  import: fold
  package: set-size-1.16
}

main {
  import: def
  import: thm
  import: finite
  import: fold
  import: size
}

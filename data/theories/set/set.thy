name: set
version: 1.13
description: Standard set theory
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: set-def-1.14
}

thm {
  import: def
  package: set-thm-1.14
}

finite {
  import: def
  import: thm
  package: set-finite-1.11
}

fold {
  import: def
  import: thm
  import: finite
  package: set-fold-1.9
}

size {
  import: def
  import: thm
  import: finite
  import: fold
  package: set-size-1.11
}

main {
  import: def
  import: thm
  import: finite
  import: fold
  import: size
}

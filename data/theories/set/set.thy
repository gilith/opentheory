name: set
version: 1.5
description: Set theory
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"
show: "Set"

def {
  package: set-def-1.7
}

thm {
  import: def
  package: set-thm-1.6
}

finite {
  import: def
  import: thm
  package: set-finite-1.3
}

fold {
  import: def
  import: thm
  import: finite
  package: set-fold-1.2
}

size {
  import: def
  import: thm
  import: finite
  import: fold
  package: set-size-1.3
}

main {
  import: def
  import: thm
  import: finite
  import: fold
  import: size
}

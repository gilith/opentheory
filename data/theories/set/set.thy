name: set
version: 1.1
description: Set theory
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Set"

def {
  package: set-def-1.2
}

thm {
  import: def
  package: set-thm-1.1
}

finite {
  import: def
  import: thm
  package: set-finite-1.0
}

fold {
  import: def
  import: thm
  import: finite
  package: set-fold-1.0
}

size {
  import: def
  import: thm
  import: finite
  import: fold
  package: set-size-1.0
}

main {
  import: def
  import: thm
  import: finite
  import: fold
  import: size
}

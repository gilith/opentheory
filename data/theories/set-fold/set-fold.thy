name: set-fold
version: 1.0
description: A fold operation on finite sets
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Set"

def {
  package: set-fold-def-1.0
}

thm {
  import: def
  package: set-fold-thm-1.0
}

main {
  import: def
  import: thm
}

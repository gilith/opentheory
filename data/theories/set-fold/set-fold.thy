name: set-fold
version: 1.4
description: A fold operation on finite sets
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Set"

def {
  package: set-fold-def-1.8
}

thm {
  import: def
  package: set-fold-thm-1.8
}

main {
  import: def
  import: thm
}

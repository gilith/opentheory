name: set-fold
version: 1.25
description: A fold operation on finite sets
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: set-thm
requires: set-finite
show: "Data.Bool"
show: "Number.Natural"
show: "Set"

def {
  package: set-fold-def-1.30
}

thm {
  import: def
  package: set-fold-thm-1.21
}

main {
  import: def
  import: thm
}

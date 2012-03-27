name: set-fold
version: 1.29
description: A fold operation on finite sets
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: set-finite
requires: set-thm
show: "Data.Bool"
show: "Number.Natural"
show: "Set"

def {
  package: set-fold-def-1.34
}

thm {
  import: def
  package: set-fold-thm-1.25
}

main {
  import: def
  import: thm
}

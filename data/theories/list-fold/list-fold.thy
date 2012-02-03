name: list-fold
version: 1.3
description: List fold operations
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list-def
requires: list-append
requires: list-reverse
show: "Data.Bool"
show: "Data.List"
show: "Function"

def {
  package: list-fold-def-1.2
}

thm {
  import: def
  package: list-fold-thm-1.2
}

main {
  import: def
  import: thm
}

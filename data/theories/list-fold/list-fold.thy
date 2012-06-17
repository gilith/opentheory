name: list-fold
version: 1.12
description: List fold operations
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list-append
requires: list-def
requires: list-length
requires: list-reverse
requires: natural
show: "Data.Bool"
show: "Data.List"
show: "Function"
show: "Number.Natural"

def {
  package: list-fold-def-1.11
}

thm {
  import: def
  package: list-fold-thm-1.13
}

main {
  import: def
  import: thm
}

name: list-filter
version: 1.24
description: The list filter function
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: natural
requires: set
requires: list-def
requires: list-length
requires: list-set
requires: list-append
requires: list-map
show: "Data.Bool"
show: "Data.List"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: list-filter-def-1.20
}

thm {
  import: def
  package: list-filter-thm-1.25
}

main {
  import: def
  import: thm
}

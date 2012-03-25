name: list-filter
version: 1.34
description: The list filter function
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list-append
requires: list-def
requires: list-length
requires: list-map
requires: list-set
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: list-filter-def-1.29
}

thm {
  import: def
  package: list-filter-thm-1.35
}

main {
  import: def
  import: thm
}

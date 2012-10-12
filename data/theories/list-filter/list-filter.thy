name: list-filter
version: 1.45
description: The list filter function
author: Joe Leslie-Hurd <joe@gilith.com>
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
  package: list-filter-def-1.41
}

thm {
  import: def
  package: list-filter-thm-1.48
}

main {
  import: def
  import: thm
}

name: list-map
version: 1.26
description: The list map function
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: set
requires: list-def
requires: list-thm
requires: list-length
requires: list-set
requires: list-append
show: "Data.Bool"
show: "Data.List"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: list-map-def-1.22
}

thm {
  import: def
  package: list-map-thm-1.28
}

main {
  import: def
  import: thm
}

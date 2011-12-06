name: list-member
version: 1.32
description: The list member function
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
requires: list-quant
requires: list-filter
requires: list-reverse
requires: list-nth
show: "Data.Bool"
show: "Data.List"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: list-member-def-1.24
}

thm {
  import: def
  package: list-member-thm-1.34
}

main {
  import: def
  import: thm
}

name: list-replicate
version: 1.57
description: The list replicate function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-append
requires: list-def
requires: list-length
requires: list-map
requires: list-nth
requires: list-set
requires: list-thm
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-replicate-def-1.48
}

thm {
  import: def
  package: list-replicate-thm-1.58
}

main {
  import: def
  import: thm
}

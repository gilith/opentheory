name: list-reverse
version: 1.43
description: The list reverse function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-append
requires: list-def
requires: list-length
requires: list-map
requires: list-set
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-reverse-def-1.43
}

thm {
  import: def
  package: list-reverse-thm-1.15
}

main {
  import: def
  import: thm
}

name: list-reverse
version: 1.33
description: The list reverse function
author: Joe Hurd <joe@gilith.com>
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
  package: list-reverse-def-1.31
}

thm {
  import: def
  package: list-reverse-thm-1.14
}

main {
  import: def
  import: thm
}

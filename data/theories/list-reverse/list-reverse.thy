name: list-reverse
version: 1.21
description: The list reverse function
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: set
requires: list-def
requires: list-length
requires: list-set
requires: list-append
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-reverse-def-1.20
}

thm {
  import: def
  package: list-reverse-thm-1.10
}

main {
  import: def
  import: thm
}

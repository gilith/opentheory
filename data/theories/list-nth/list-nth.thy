name: list-nth
version: 1.49
description: The list nth function
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-append
requires: list-def
requires: list-dest
requires: list-last
requires: list-length
requires: list-map
requires: list-set
requires: list-thm
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-nth-def-1.45
}

thm {
  import: def
  package: list-nth-thm-1.52
}

main {
  import: def
  import: thm
}

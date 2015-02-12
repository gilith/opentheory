name: list-interval
version: 1.61
description: The list interval function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-length
requires: list-map
requires: list-nth
requires: natural
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-interval-def-1.59
}

thm {
  import: def
  package: list-interval-thm-1.60
}

main {
  import: def
  import: thm
}

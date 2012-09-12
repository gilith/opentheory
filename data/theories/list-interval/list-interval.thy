name: list-interval
version: 1.50
description: The list interval function
author: Joe Hurd <joe@gilith.com>
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
  package: list-interval-def-1.48
}

thm {
  import: def
  package: list-interval-thm-1.52
}

main {
  import: def
  import: thm
}

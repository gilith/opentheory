name: list-interval
version: 1.56
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
  package: list-interval-def-1.54
}

thm {
  import: def
  package: list-interval-thm-1.57
}

main {
  import: def
  import: thm
}

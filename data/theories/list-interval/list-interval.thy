name: list-interval
version: 1.28
description: The list interval function
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: list-length
requires: list-nth
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-interval-def-1.30
}

thm {
  import: def
  package: list-interval-thm-1.28
}

main {
  import: def
  import: thm
}

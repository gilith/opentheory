name: list-append
version: 1.25
description: The list append function
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: set
requires: list-def
requires: list-thm
requires: list-dest
requires: list-length
requires: list-set
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-append-def-1.22
}

thm {
  import: def
  package: list-append-thm-1.14
}

main {
  import: def
  import: thm
}
